import coremltools as ct
import struct
import socket
import numpy as np
import tensorflow as tf
from tensorflow.keras.models import Model
from tensorflow.keras.layers import Dense, Flatten, Reshape, Input, Conv3D, Conv2D, MaxPooling2D, Concatenate, Lambda
from tensorflow.keras.layers import MaxPooling3D, Dropout, BatchNormalization, LeakyReLU, Add
import random
from collections import deque
import matplotlib.pyplot as plt
import threading
import queue
import time

BOARD_SIZE = 6
STACK_DEPTH = BOARD_SIZE + 1
BITBOARD_PLANES = 5
TURN_PLANES = 1
INPUT_CHANNELS = STACK_DEPTH + BITBOARD_PLANES + TURN_PLANES
TOTAL_INPUT = BOARD_SIZE * BOARD_SIZE * (STACK_DEPTH + BITBOARD_PLANES + TURN_PLANES)
POLICY_SIZE = 65
TOTAL_OUTPUT = 66

@tf.function
def value_entropy_loss(y_true, y_pred):
    mse = tf.reduce_mean(tf.square(y_true - y_pred))

    eps = tf.keras.backend.epsilon()
    var_pen = 0.01 * tf.reduce_mean(1.0 / (tf.abs(y_pred) + eps))

    y_flat = tf.reshape(y_pred, [-1])
    y_flat = tf.abs(y_flat)
    hist  = tf.histogram_fixed_width(y_flat, [0.0, 1.0], nbins=10)
    probs = tf.cast(hist, tf.float32) / tf.reduce_sum(tf.cast(hist, tf.float32))
    entropy = -tf.reduce_sum(probs * tf.math.log(probs + eps))
    ent_pen = 0.5 * (1.0 / (entropy + eps))

    return mse + var_pen + ent_pen

class ExperienceBuffer:
    def __init__(self, max_size=10000):
        self.buffer = deque(maxlen=max_size)
        self.priorities = deque(maxlen=max_size)
        self.value_distribution = {
                "high_pos": 0,   # > 0.6
                "mid_pos": 0,    # 0.2 to 0.6
                "low_pos": 0,    # 0 to 0.2
                "low_neg": 0,    # -0.2 to 0
                "mid_neg": 0,    # -0.6 to -0.2
                "high_neg": 0    # < -0.6
                }
        self.lock = threading.Lock()

    def add(self, inputs, targets):
        value = targets[0][0]

        if value > 0.6:
            self.value_distribution["high_pos"] += 1
        elif value > 0.2:
            self.value_distribution["mid_pos"] += 1
        elif value > 0:
            self.value_distribution["low_pos"] += 1
        elif value > -0.2:
            self.value_distribution["low_neg"] += 1
        elif value > -0.6:
            self.value_distribution["mid_neg"] += 1
        else:
            self.value_distribution["high_neg"] += 1

        priority = 1.0 + abs(value)

        with self.lock:
            self.buffer.append((inputs, targets))
            self.priorities.append(priority)

            # inverse_inputs = -inputs
            # inverse_targets = targets.copy()
            # inverse_targets[0] = -targets[0]
            #
            # self.buffer.append((inverse_inputs, inverse_targets))
            # self.priorities.append(priority)

    def sample(self, batch_size):
        with self.lock:
            if batch_size > len(self.buffer):
                batch_size = len(self.buffer)

            if len(self.buffer) == 0:
                return None, None

            probs = np.array(self.priorities) / sum(self.priorities)

            indices = np.random.choice(len(self.buffer), size=batch_size, p=probs, replace=False)

            batch = [self.buffer[idx] for idx in indices]
            inputs = np.vstack([item[0] for item in batch])

            # Extract values and policies separately for proper formatting
            values = np.array([item[1][0][0] for item in batch]).reshape(-1, 1)
            policies = np.vstack([item[1][0][1:] for item in batch])

            # Return formatted targets as a dictionary
            formatted_targets = {
                    'value_head': values,
                    'policy_head': policies
                    }

            return inputs, formatted_targets
    
    def get_distribution_stats(self):
        total = sum(self.value_distribution.values())
        if total == 0:
            return "No data yet"

        dist = {k: v/total for k, v in self.value_distribution.items()}
        return dist

    def size(self):
        with self.lock:
            return len(self.buffer)

class TrainingItem:
    def __init__(self, inputs, targets, is_td=False):
        self.inputs = inputs
        self.targets = targets
        self.is_td = is_td

class NeuralNetworkTrainer:
    def __init__(self):
        self.model = self._create_unified_model()

        self.training_queue = queue.Queue()
        self.running = True
        self.model_lock = threading.Lock()

        self.experience_buffer = ExperienceBuffer(max_size=20000)
        self.replay_batch_size = 128
        self.train_counter = 0
        self.replay_frequency = 5
        self.last_was_train = 0
        self.game_counter = 0
        self.game_end = False

        self.game_input_buffer = []
        self.game_value_buffer = []
        self.game_buffer_lock = threading.Lock()

        self.initial_lr = 0.001
        self.min_lr = 0.00001

        self.value_predictions = []

        self._load_model()

        self._convert_to_coreml()

        self.training_thread = threading.Thread(target=self._training_worker)
        self.training_thread.daemon = True
        self.training_thread.start()

    def _residual_block(self, x, filters):
        shortcut = x

        x = Conv2D(filters, (3, 3), padding='same', activation='sigmoid', kernel_regularizer=tf.keras.regularizers.l2(0.01))(x)
        x = BatchNormalization()(x)
        x = LeakyReLU(alpha=0.1)(x)

        x = Conv2D(filters, (3, 3), padding='same', activation='sigmoid', kernel_regularizer=tf.keras.regularizers.l2(0.01))(x)
        x = BatchNormalization()(x)

        x = Add()([x, shortcut])
        x = LeakyReLU(alpha=0.1)(x)

        return x

    def _create_unified_model(self):
        inputs = Input(shape=(TOTAL_INPUT,), name='input')
        x = Reshape((BOARD_SIZE, BOARD_SIZE, INPUT_CHANNELS), name='reshape')(inputs)

        x = Conv2D(256, (3, 3), padding='same', name='conv_init', activation='sigmoid', kernel_regularizer=tf.keras.regularizers.l2(0.01))(x)
        x = BatchNormalization(name='bn_init')(x)
        x = LeakyReLU(alpha=0.1, name='act_init')(x)

        x = self._residual_block(x, 256)
        x = self._residual_block(x, 256)
        x = Dropout(0.25)(x)
        x = self._residual_block(x, 256)
        x = self._residual_block(x, 256)
        x = Dropout(0.25)(x)

        x = Conv2D(256, (1, 1), padding='same', name='conv_final', activation='sigmoid', kernel_regularizer=tf.keras.regularizers.l2(0.01))(x)
        x = Flatten(name='flatten_shared')(x)

        combined_out = Dense(1 + POLICY_SIZE, name='combined_head')(x)

        value_out = Lambda(lambda x: tf.nn.tanh(x[:, 0:1]), name='value_head')(combined_out)

        policy_out = Lambda(lambda x: tf.nn.tanh(x[:, 1:]), name='policy_head')(combined_out)

        model = Model(inputs=inputs, outputs=[combined_out, policy_out, value_out], name='bnn_model_res')

        losses = {
                # 'combined_head': "mean_absolute_error",
                'policy_head': 'categorical_crossentropy',
                'value_head': value_entropy_loss,
                }

        loss_weights = {
                'policy_head': 0.1, 
                'value_head': 1.0,
                }

        model.compile(
                optimizer=tf.keras.optimizers.legacy.Adam(learning_rate=0.00001, clipnorm=0.5),
                loss=losses,
                loss_weights=loss_weights,
                metrics={'policy_head': 'mean_absolute_error', 'value_head': 'accuracy'}
                )

        model.summary()
        return model 

    def _load_model(self):
        try:
            self.model = tf.keras.models.load_model('neurelnet_unified.h5')
            print("Loaded existing unified model")
        except (FileNotFoundError, OSError):
            print("No existing unified model found, using new model")
        except Exception as e:
            print(f"Error loading model: {e}")

    def _convert_to_coreml(self):
        try:
            coreml_model = ct.convert(
                    self.model, 
                    inputs=[ct.TensorType(shape=(1, TOTAL_INPUT))]
                    )
            coreml_model.save('neurelnet.mlpackage')
            print("Converted and saved CoreML model")
        except Exception as e:
            print(f"Error converting to CoreML: {e}")

    def save_models(self):
        with self.model_lock:
            try:
                self.model.save('neurelnet_unified.h5')
                coreml_model = ct.convert(
                        self.model,
                        inputs=[ct.TensorType(shape=(1, TOTAL_INPUT))],
                        )
                coreml_model.save('neurelnet.mlpackage')
                print("Saved models")
            except Exception as e:
                print(f"Error saving model: {e}")

    def plot_value_distribution(self, values, filename='value_distribution.png'):
        plt.figure(figsize=(10, 6))
        plt.hist(values, bins=20, alpha=0.7)
        plt.title('Distribution of Value Predictions')
        plt.xlabel('Value')
        plt.ylabel('Frequency')
        plt.axvline(x=0, color='r', linestyle='--')
        plt.grid(True, alpha=0.3)
        plt.savefig(filename)
        plt.close()

    def process_game_end(self):
        with self.game_buffer_lock:
            if len(self.game_input_buffer) > 0:
                self.game_counter += 1
                inputs = np.vstack(self.game_input_buffer)
                targets = np.vstack(self.game_value_buffer)
                self.game_input_buffer.clear()
                self.game_value_buffer.clear()

                self.training_queue.put(TrainingItem(inputs, targets))

                print(f"Game {self.game_counter} queued for processing")

    def adjust_learning_rate(self):
        current_lr = max(self.initial_lr * (0.975 ** (self.train_counter // 1000)), self.min_lr)
        with self.model_lock:
            tf.keras.backend.set_value(self.model.optimizer.learning_rate, current_lr)
        print(f"Learning rate adjusted to {current_lr}")

    def train_on_replay_buffer(self, num_batches=50):
        if self.experience_buffer.size() < self.replay_batch_size:
            print("Not enough samples in buffer for replay training")
            return

        print(f"Training on experience buffer ({num_batches} batches)...")
        for i in range(num_batches):
            print(f"Training batch {i + 1}/{num_batches}")
            replay_inputs, replay_targets = self.experience_buffer.sample(self.replay_batch_size)
            if replay_inputs is None:
                break

            with self.model_lock:
                # replay_targets is now a dictionary with the correct format
                self.model.train_on_batch(x=replay_inputs, y=replay_targets)
    
    def test_model_performance(self):
        pass
        # if self.experience_buffer.size() < self.replay_batch_size:
        #     print("Not enough samples in buffer for testing")
        #     return
        #
        # test_inputs, test_targets = self.experience_buffer.sample(self.replay_batch_size)
        # if test_inputs is None:
        #     return
        #
        # with self.model_lock:
        #     losses = self.model.evaluate(x=test_inputs, y=test_targets, verbose=0)
        #     print(f"Test losses: {losses}")
        #
        #     predictions = self.model.predict(test_inputs, verbose=0)
        #     value_preds = predictions[:, 0].flatten()
        #
        # avg_abs_value = np.mean(np.abs(value_preds))
        # print(f"Test value stats - Mean abs: {avg_abs_value:.4f}, Range: {np.min(value_preds):.4f} to {np.max(value_preds):.4f}")

        # self.plot_value_distribution(value_preds, f'test_values_{self.train_counter}.png')

    def signal_game_end(self):
        self.game_end = True
        self.process_game_end()

    def predict(self, inputs):
        self.last_was_train += 1

        if self.game_end and len(self.game_input_buffer) > 0:
            self.process_game_end()
            self.game_end = False

        with self.model_lock:
            combined_output = self.model.predict(inputs, verbose=0)

        prediction = combined_output[0]
        value_pred = prediction[0]

        self.value_predictions.append(value_pred)

        if len(self.value_predictions) % 100 == 0:
            recent_preds = self.value_predictions[-100:]
            avg_abs_value = np.mean(np.abs(recent_preds))
            print(f"Recent value prediction stats - Mean abs: {avg_abs_value:.4f}")

            if avg_abs_value < 0.01:
                print("WARNING: Possible model collapse detected - values too close to zero")

            if len(self.value_predictions) % 1000 == 0:
                # self.plot_value_distribution(self.value_predictions[-1000:], 
                                             # f'value_dist_{len(self.value_predictions)}.png')
                print(f"Buffer distribution: {self.experience_buffer.get_distribution_stats()}")

        # print(f"Value prediction: {value_pred}")

        return prediction

    def train(self, inputs, targets):
        if np.any(np.isnan(inputs)) or np.any(np.isinf(inputs)):
            print("WARNING: NaN or Inf detected in inputs, skipping training")
            return

        if np.any(np.isnan(targets)) or np.any(np.isinf(targets)):
            print("WARNING: NaN or Inf detected in targets, skipping training")
            return
        self.game_end = False
        self.last_was_train += 1

        value_target = targets[0][0]
        policy_target = targets[0][1:]

        # Format targets correctly for model training
        formatted_targets = {
            'policy_head': policy_target.reshape(1, -1),
            'value_head': np.array([[value_target]])
        }

        with self.game_buffer_lock:
            self.game_input_buffer.append(inputs)
            self.game_value_buffer.append(targets)

        self.experience_buffer.add(inputs, targets)

        # Put properly formatted targets in the queue
        # self.training_queue.put(TrainingItem(inputs, formatted_targets, is_td=False))

    def train_td(self, inputs, targets):
        value_target = targets[0][0]
        policy_target = targets[0][1:]
        
        # Format targets correctly for model training
        formatted_targets = {
            'policy_head': policy_target.reshape(1, -1),
            'value_head': np.array([[value_target]])
        }
        
        self.training_queue.put(TrainingItem(inputs, formatted_targets, is_td=True))
    
    def shutdown(self):
        """Shutdown the training thread"""
        self.running = False
        if self.training_thread.is_alive():
            self.training_thread.join(timeout=5.0)
        self.save_models()

    def _training_worker(self):
        print("Training worker thread started")
        use_queue = True

        while self.running:
            # try:
            if use_queue:
                try:
                    if self.training_queue.qsize() > 6:
                        # self._process_training_queue()
                        # print(f"Training queue size: {self.training_queue.qsize()}")
                        for _ in range(5):
                            item = self.training_queue.get(timeout=1.0)
                            self._process_training_item(item)
                            self.training_queue.task_done()
                        if self.training_queue.qsize() < 150:
                            use_queue = False
                    else:
                        use_queue = False
                except queue.Empty:
                    if self.experience_buffer.size() >= self.replay_batch_size:
                        self.train_on_replay_buffer(4)
                    sleep(0.1)
            else:
                if self.experience_buffer.size() >= self.replay_batch_size:
                    self.train_on_replay_buffer(4)
                    use_queue = True


            # except Exception as e:
                # print(f"Error in training worker: {e}")
                # time.sleep(1.0)

    def _process_training_queue(self):
        # batch 128 items from the queue and train
        batch = [self.training_queue.get() for _ in range(128)]
        self.training_queue.task_done()
        inputs = np.vstack([item.inputs for item in batch])
        
        value_targets = np.array([item.targets['value_head'] for item in batch])
        policy_targets = np.vstack([item.targets['policy_head'] for item in batch])
        formatted_targets = {
            'value_head': value_targets,
            'policy_head': policy_targets
        }
        with self.model_lock:
            self.model.train_on_batch(x=inputs, y=formatted_targets)

        self.train_counter += 1
        if self.train_counter % 250 == 0:
            self.adjust_learning_rate()
        if self.train_counter % 10 == 0:
            print(f"Saving model at training step {self.train_counter}")
            self.save_models()


    def _process_training_item(self, item):
        print(f"Processing training item: {item.is_td}")
        if item.is_td:
            with self.model_lock:
                value_target = item.targets['value_head']
                predictions = self.model.predict(item.inputs, verbose=0)

                td_targets = {
                    'value_head': value_target,
                    'policy_head': predictions[1]
                }

                self.model.train_on_batch(x=item.inputs, y=td_targets)
                # inverse_inputs = -item.inputs
                # inverse_value_targets = -value_target
                #
                # inverse_predictions = self.model.predict(inverse_inputs, verbose=0)
                # inverse_td_targets = {
                #     'value_head': inverse_value_targets,
                #     'policy_head': inverse_predictions[1]
                # }
                #
                # self.model.train_on_batch(x=inverse_inputs, y=inverse_td_targets)
        else:
            with self.model_lock:
                # For non-TD items, ensure we're always providing dictionary targets
                if isinstance(item.targets, dict):
                    # Already in correct format
                    self.model.train_on_batch(x=item.inputs, y=item.targets)
                else:
                    # Need to convert to dictionary format
                    value_target = item.targets[:, 0:1]
                    policy_target = item.targets[:, 1:]
                    
                    formatted_targets = {
                        'value_head': value_target,
                        'policy_head': policy_target
                    }
                    
                    self.model.train_on_batch(x=item.inputs, y=formatted_targets)
                    
                    # Train on inverse data for symmetry
                    # if len(item.inputs.shape) == 2 and item.inputs.shape[0] == 1:
                    #     inverse_inputs = -item.inputs.copy()
                    #     inverse_targets = {
                    #         'value_head': -value_target,
                    #         'policy_head': policy_target
                    #     }
                    #     self.model.train_on_batch(x=inverse_inputs, y=inverse_targets)

            self.train_counter += 1
            if self.train_counter % 100 == 0:
                self.adjust_learning_rate()
            if self.train_counter % 10 == 0:
                print(f"Saving model at training step {self.train_counter}")
                self.save_models()
                if self.train_counter % 1000 == 0:
                    self.test_model_performance() 

def recv_all(conn, n):
    data = b''
    while len(data) < n:
        packet = conn.recv(n - len(data))
        if not packet:
            return None
        data += packet
    return data

def receive_data(conn):
    size_data = recv_all(conn, 4)
    if size_data is None:
        return None
    size = struct.unpack('!I', size_data)[0]
    return recv_all(conn, size)

def send_data(conn, data):
    size = len(data)
    conn.sendall(struct.pack('!I', size))
    conn.sendall(data)

def send_ack(conn):
    conn.sendall(b'ACK\0')

def wait_for_ack(conn):
    ack = recv_all(conn, 4)
    if ack is None:
        return False
    if ack != b'ACK\0':
        return False
    return True

def handle_client(conn, addr, trainer):
    print(f"Connection from {addr}")
    try:
        while True:
            print(f"[{addr}] Waiting for request...")
            header = receive_data(conn)
            if header is None:
                print(f"[{addr}] Client closed connection or partial header read")
                break

            send_ack(conn)

            request_type = header.decode('utf-8', errors='ignore').rstrip('\x00')
            print(f"[{addr}] Received request type: {request_type}")

            if request_type == 'predict':
                size_bytes = receive_data(conn)
                if size_bytes is None:
                    print(f"[{addr}] Failed to read input_size prefix")
                    break
                send_ack(conn)

                input_size = struct.unpack('i', size_bytes)[0]
                if input_size != TOTAL_INPUT:
                    print(f"[{addr}] Input size mismatch ({input_size} != {TOTAL_INPUT})")
                    conn.sendall(struct.pack('i', 0))
                    continue

                raw = receive_data(conn)
                if raw is None:
                    print(f"[{addr}] Incomplete input data")
                    break
                send_ack(conn)

                inputs = np.frombuffer(raw, dtype=np.float64).reshape(1, -1)

                prediction = trainer.predict(inputs)

                prediction_bytes = struct.pack('!66f', *prediction)
                conn.sendall(prediction_bytes)

                if not wait_for_ack(conn):
                    print(f"[{addr}] Failed to receive ACK after sending prediction")
                    break

                print(f"[{addr}] Prediction cycle completed successfully")

            elif request_type == 'train':
                batch = 1

                raw_inputs = receive_data(conn)
                if raw_inputs is None:
                    print(f"[{addr}] Incomplete training inputs")
                    break
                send_ack(conn)

                raw_targets = receive_data(conn)
                if raw_targets is None:
                    print(f"[{addr}] Incomplete training targets")
                    break

                inputs = np.frombuffer(raw_inputs, dtype=np.float64).reshape(batch, TOTAL_INPUT)
                targets = np.frombuffer(raw_targets, dtype=np.float64).reshape(batch, TOTAL_OUTPUT)
                value_target = targets[0][0]
                print(f"[{addr}] Training value target: {value_target}")

                trainer.train(inputs, targets)

                send_ack(conn)
                print(f"[{addr}] Training data queued successfully")

            elif request_type == 'trainTD':
                batch = 1

                raw_inputs = receive_data(conn)
                if raw_inputs is None:
                    print(f"[{addr}] Incomplete training inputs")
                    break
                send_ack(conn)

                raw_targets = receive_data(conn)
                if raw_targets is None:
                    print(f"[{addr}] Incomplete training targets")
                    break

                inputs = np.frombuffer(raw_inputs, dtype=np.float64).reshape(batch, TOTAL_INPUT)
                targets = np.frombuffer(raw_targets, dtype=np.float64).reshape(batch, TOTAL_OUTPUT)

                trainer.train_td(inputs, targets)

                send_ack(conn)
                print(f"[{addr}] TD Training data queued successfully")

            elif request_type == 'game_end':
                # Signal that a game has ended
                trainer.signal_game_end()
                send_ack(conn)
                print(f"[{addr}] Game end signal processed")

            else:
                print(f"[{addr}] Unknown request: {request_type}")
                break
    except Exception as e:
        print(f"[{addr}] Error handling client: {e}")
    finally:
        conn.close()
        print(f"[{addr}] Connection closed")

def main():
    trainer = NeuralNetworkTrainer()
    print("AlphaZero-style unified model ready, listening for connections...")

    HOST, PORT = 'localhost', 65432
    active_threads = []
    max_connections = 4

    with socket.socket(socket.AF_INET, socket.SOCK_STREAM) as server:
        server.setsockopt(socket.SOL_SOCKET, socket.SO_REUSEADDR, 1)
        server.bind((HOST, PORT))
        server.listen()

        server.settimeout(1.0)

        try:
            while True:
                active_threads = [t for t in active_threads if t.is_alive()]

                if len(active_threads) < max_connections:
                    try:
                        conn, addr = server.accept()
                        client_thread = threading.Thread(
                                target=handle_client,
                                args=(conn, addr, trainer),
                                daemon=True
                                )
                        client_thread.start()
                        active_threads.append(client_thread)
                        print(f"New client thread started. Active connections: {len(active_threads)}/{max_connections}")
                    except socket.timeout:
                        pass
                    except Exception as e:
                        print(f"Error accepting connection: {e}")
                else:
                    time.sleep(0.1)

        except KeyboardInterrupt:
            print("Server shutting down...")
        finally:
            for thread in active_threads:
                thread.join(timeout=2.0)

            print("Shutting down trainer and saving model...")
            trainer.shutdown()
            print("Server shutdown complete")

if __name__ == '__main__':
    main()
