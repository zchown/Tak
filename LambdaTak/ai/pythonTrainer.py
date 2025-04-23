import coremltools as ct
import struct
import socket
import numpy as np
import tensorflow as tf
from tensorflow.keras.models import Model
from tensorflow.keras.layers import Dense, Flatten, Reshape, Input, Conv3D, Conv2D, MaxPooling2D
from tensorflow.keras.layers import MaxPooling3D, Dropout, BatchNormalization, Concatenate, LeakyReLU, Add
import random
from collections import deque
import matplotlib.pyplot as plt

# Model configuration
TOTAL_INPUT = 6 * 6 * 7
INPUT_SQUARES = 36
ROW_SIZE = 6
INPUT_SQUARE_DEPTH = 7
INPUT_PIECE_TYPES = 3
POLICY_SIZE = 65
TOTAL_OUTPUT = 66

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

        self.buffer.append((inputs, targets))
        self.priorities.append(priority)

        inverse_inputs = -inputs
        inverse_targets = targets.copy()
        inverse_targets[0] = -targets[0]

        self.buffer.append((inverse_inputs, inverse_targets))
        self.priorities.append(priority)

    def sample(self, batch_size):
        if batch_size > len(self.buffer):
            batch_size = len(self.buffer)

        probs = np.array(self.priorities) / sum(self.priorities)

        # Sample based on priorities
        indices = np.random.choice(len(self.buffer), size=batch_size, p=probs, replace=False)

        batch = [self.buffer[idx] for idx in indices]
        inputs = np.vstack([item[0] for item in batch])
        targets = np.vstack([item[1] for item in batch])
        return inputs, targets

    def get_distribution_stats(self):
        total = sum(self.value_distribution.values())
        if total == 0:
            return "No data yet"

        dist = {k: v/total for k, v in self.value_distribution.items()}
        return dist

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

def value_entropy_loss(y_true, y_pred):
    mse = tf.keras.losses.mean_squared_error(y_true, y_pred)
    
    eps = 1e-5
    # variance_penalty = 0.005 * tf.reduce_mean(1.0 / (tf.abs(y_pred) + eps))
    variance_penalty = 0;
    
    y_pred_flat = tf.reshape(y_pred, [-1])
    histogram = tf.histogram_fixed_width(y_pred_flat, [-1.0, 1.0], nbins=10)
    histogram = tf.cast(histogram, tf.float32) / tf.reduce_sum(tf.cast(histogram, tf.float32))
    entropy = -tf.reduce_sum(histogram * tf.math.log(histogram + eps))
    
    entropy_penalty = 0.05 * (1.0 / (entropy + eps))
    
    return mse + variance_penalty + entropy_penalty

def residual_block(x, filters):
    shortcut = x

    x = Conv2D(filters, (3, 3), padding='same', kernel_regularizer=tf.keras.regularizers.l2(0.0005))(x)
    x = BatchNormalization()(x)
    x = LeakyReLU(alpha=0.1)(x)

    x = Conv2D(filters, (3, 3), padding='same', kernel_regularizer=tf.keras.regularizers.l2(0.0005))(x)
    x = BatchNormalization()(x)

    x = Add()([x, shortcut])
    x = LeakyReLU(alpha=0.1)(x)

    return x

def create_alphazero_model():
    input_layer = Input(shape=(TOTAL_INPUT,), name='input')
    x = Reshape((ROW_SIZE, ROW_SIZE, INPUT_SQUARE_DEPTH))(input_layer)

    x = Conv2D(128, (3, 3), activation='relu', padding='same', kernel_regularizer=tf.keras.regularizers.l2(0.0005))(x)
    x = BatchNormalization()(x)
    x = LeakyReLU(alpha=0.1)(x)

    x = Conv2D(256, (3, 3), padding='same', kernel_regularizer=tf.keras.regularizers.l2(0.0005))(x)
    x = BatchNormalization()(x)
    x = LeakyReLU(alpha=0.1)(x)

    x = residual_block(x, 256)
    x = residual_block(x, 256)
    x = residual_block(x, 256)
    x = residual_block(x, 256)
    x = residual_block(x, 256)
    x = residual_block(x, 256)
    x = residual_block(x, 256)
    x = residual_block(x, 256)
    x = residual_block(x, 256)
    x = residual_block(x, 256)

    x = Flatten()(x)

    x = Dense(512, kernel_regularizer=tf.keras.regularizers.l2(0.0005))(x)
    x = BatchNormalization()(x)
    x = LeakyReLU(alpha=0.1)(x)
    x = Dropout(0.3)(x)

    x = Dense(256, kernel_regularizer=tf.keras.regularizers.l2(0.0005))(x)
    x = BatchNormalization()(x)
    x = LeakyReLU(alpha=0.1)(x)
    x = Dropout(0.3)(x)

    policy_hidden = Dense(128, activation='relu')(x)
    policy_output = Dense(POLICY_SIZE, activation='softmax', name='policy_output')(policy_hidden)

    value_hidden = Dense(128)(x)
    value_hidden = BatchNormalization()(value_hidden)
    value_hidden = LeakyReLU(alpha=0.1)(value_hidden)
    value_output = Dense(1, activation='tanh', name='value_output', 
                         kernel_initializer=tf.keras.initializers.RandomNormal(mean=0.0, stddev=0.1))(value_hidden)

    internal_model = Model(inputs=input_layer, outputs=[value_output, policy_output])

    # Wrapper for CoreML conversion
    combined_output = Concatenate(name='combined_output')([value_output, policy_output])
    combined_model = Model(inputs=input_layer, outputs=combined_output)

    # Compile the internal model for training
    internal_model.compile(
            optimizer=tf.keras.optimizers.legacy.Adam(learning_rate=0.0001),
            loss={
                'value_output': value_entropy_loss,
                'policy_output': 'categorical_crossentropy'
                },
            loss_weights={
                'value_output': 3.0,  # Higher weight on value head
                'policy_output': 1.0
                },
            metrics={
                'value_output': 'mean_squared_error',
                'policy_output': 'accuracy'
                }
            )

    internal_model.summary()
    combined_model.summary()

    return internal_model, combined_model

def plot_value_distribution(values, filename='value_distribution.png'):
    plt.figure(figsize=(10, 6))
    plt.hist(values, bins=20, alpha=0.7)
    plt.title('Distribution of Value Predictions')
    plt.xlabel('Value')
    plt.ylabel('Frequency')
    plt.axvline(x=0, color='r', linestyle='--')
    plt.grid(True, alpha=0.3)
    plt.savefig(filename)
    plt.close()

def main():
    internal_model, combined_model = create_alphazero_model()
    print("AlphaZero-style model ready, listening for connections...")

    experience_buffer = ExperienceBuffer(max_size=20000)
    replay_batch_size = 128
    train_counter = 0
    replay_frequency = 5
    last_was_train = False

    value_predictions = []

    initial_lr = 0.001
    min_lr = 0.00001

    try:
        internal_model = tf.keras.models.load_model('neurelnet_internal.h5', 
                                                    custom_objects={
                                                        'value_entropy_loss': value_entropy_loss,
                                                        'LeakyReLU': LeakyReLU
                                                        })
        input_layer = internal_model.input
        value_output = internal_model.get_layer('value_output').output
        policy_output = internal_model.get_layer('policy_output').output
        combined_output = Concatenate(name='combined_output')([value_output, policy_output])
        combined_model = Model(inputs=input_layer, outputs=combined_output)
        print("Loaded existing model")
    except (FileNotFoundError, OSError):
        print("No existing model found, using new model")
    except Exception as e:
        print(f"Error loading model: {e}")
        return

    try:
        coreml_model = ct.convert(
                combined_model, 
                inputs=[ct.TensorType(shape=(1, TOTAL_INPUT))]
                )
        coreml_model.save('neurelnet.mlpackage')
        print("Converted and saved CoreML model")
    except Exception as e:
        print(f"Error converting to CoreML: {e}")
        return

    HOST, PORT = 'localhost', 65432
    with socket.socket(socket.AF_INET, socket.SOCK_STREAM) as server:
        server.setsockopt(socket.SOL_SOCKET, socket.SO_REUSEADDR, 1)
        server.bind((HOST, PORT))
        server.listen()
        while True:
            conn, addr = server.accept()
            print(f"Connection from {addr}")
            with conn:
                while True:
                    print("Waiting for request...")
                    header = receive_data(conn)
                    if header is None:
                        print("Client closed connection or partial header read")
                        break

                    send_ack(conn)

                    request_type = header.decode('utf-8', errors='ignore').rstrip('\x00')
                    print(f"Received request type: {request_type}")

                    if request_type == 'predict':
                        if last_was_train:
                            last_was_train = False
                            for _ in range(50):
                                if len(experience_buffer.buffer) < replay_batch_size * 10:
                                    break
                                replay_inputs, replay_targets = experience_buffer.sample(replay_batch_size)
                                replay_value_targets = replay_targets[:, 0:1]
                                replay_policy_targets = replay_targets[:, 1:TOTAL_OUTPUT]
                                internal_model.train_on_batch(
                                        x=replay_inputs, 
                                        y={'value_output': replay_value_targets, 'policy_output': replay_policy_targets}
                                        )

                        size_bytes = receive_data(conn)
                        if size_bytes is None:
                            print("Failed to read input_size prefix")
                            break
                        send_ack(conn)

                        input_size = struct.unpack('i', size_bytes)[0]
                        if input_size != TOTAL_INPUT:
                            print(f"Input size mismatch ({input_size} != {TOTAL_INPUT})")
                            conn.sendall(struct.pack('i', 0))
                            continue

                        raw = receive_data(conn)
                        if raw is None:
                            print("Incomplete input data")
                            break
                        send_ack(conn)

                        inputs = np.frombuffer(raw, dtype=np.float64).reshape(1, -1)

                        combined_output = combined_model.predict(inputs, verbose=0)
                        value_pred = combined_output[0][0]

                        value_predictions.append(value_pred)

                        if len(value_predictions) % 100 == 0:
                            recent_preds = value_predictions[-100:]
                            avg_abs_value = np.mean(np.abs(recent_preds))
                            print(f"Recent value prediction stats - Mean abs: {avg_abs_value:.4f}")

                            if avg_abs_value < 0.01:
                                print("WARNING: Possible model collapse detected - values too close to zero")
                            if len(value_predictions) % 1000 == 0:
                                plot_value_distribution(value_predictions[-1000:], 
                                                        f'value_dist_{len(value_predictions)}.png')
                                print(f"Buffer distribution: {experience_buffer.get_distribution_stats()}")

                        print(f"Value prediction: {value_pred}")

                        prediction_bytes = struct.pack('!66f', *combined_output[0])
                        conn.sendall(prediction_bytes)

                        if not wait_for_ack(conn):
                            print("Failed to receive ACK after sending prediction")
                            break

                        print("Prediction cycle completed successfully")

                    elif request_type == 'train':
                        last_was_train = True
                        batch = 1

                        raw_inputs = receive_data(conn)
                        if raw_inputs is None:
                            print("Incomplete training inputs")
                            break
                        send_ack(conn)

                        inputs = np.frombuffer(raw_inputs, dtype=np.float64).reshape(batch, TOTAL_INPUT)

                        raw_outputs = receive_data(conn)
                        if raw_outputs is None:
                            print("Incomplete training outputs")
                            break
                        send_ack(conn)

                        raw_targets = receive_data(conn)
                        if raw_targets is None:
                            print("Incomplete training targets")
                            break

                        targets = np.frombuffer(raw_targets, dtype=np.float64).reshape(batch, TOTAL_OUTPUT)

                        experience_buffer.add(inputs, targets)

                        value_targets = targets[:, 0:1]
                        policy_targets = targets[:, 1:TOTAL_OUTPUT] 

                        internal_model.train_on_batch(
                                x=inputs, 
                                y={'value_output': value_targets, 'policy_output': policy_targets}
                                )

                        inverse_inputs = -inputs
                        inverse_targets = targets.copy()
                        inverse_targets[0][0] = -targets[0][0] 
                        internal_model.train_on_batch(
                                x=inverse_inputs, 
                                y={'value_output': inverse_targets[:, 0:1], 'policy_output': inverse_targets[:, 1:TOTAL_OUTPUT]}
                                )

                        train_counter += 1

                        if train_counter % 1000 == 0:
                            current_lr = max(initial_lr * (0.95 ** (train_counter // 1000)), min_lr)
                            tf.keras.backend.set_value(internal_model.optimizer.learning_rate, current_lr)
                            print(f"Learning rate adjusted to {current_lr}")

                        if train_counter % 250 == 0:
                            # Save models
                            try:
                                internal_model.save('neurelnet_internal.h5')
                                tf.keras.models.save_model(combined_model, 'neurelnet_combined.h5')
                                coreml_model = ct.convert(
                                        combined_model,
                                        inputs=[ct.TensorType(shape=(1, TOTAL_INPUT))],
                                        )
                                coreml_model.save('neurelnet.mlpackage')
                                print("Saved models")
                            except Exception as e:
                                print(f"Error saving model: {e}")

                        if train_counter % replay_frequency == 0 and len(experience_buffer.buffer) >= replay_batch_size:
                            replay_inputs, replay_targets = experience_buffer.sample(replay_batch_size)

                            replay_value_targets = replay_targets[:, 0:1]
                            replay_policy_targets = replay_targets[:, 1:TOTAL_OUTPUT]

                            internal_model.train_on_batch(
                                    x=replay_inputs, 
                                    y={'value_output': replay_value_targets, 'policy_output': replay_policy_targets}
                                    )

                        if train_counter % 1000 == 0 and train_counter > 10000:
                            for _ in range(200):
                                if len(experience_buffer.buffer) < replay_batch_size:
                                    break

                                replay_inputs, replay_targets = experience_buffer.sample(replay_batch_size)
                                replay_value_targets = replay_targets[:, 0:1]
                                replay_policy_targets = replay_targets[:, 1:TOTAL_OUTPUT]

                                internal_model.train_on_batch(
                                        x=replay_inputs, 
                                        y={'value_output': replay_value_targets, 'policy_output': replay_policy_targets}
                                        )

                            test_inputs, test_targets = experience_buffer.sample(replay_batch_size)
                            test_value_targets = test_targets[:, 0:1]
                            test_policy_targets = test_targets[:, 1:TOTAL_OUTPUT]
                            losses = internal_model.evaluate(
                                    x=test_inputs, 
                                    y={'value_output': test_value_targets, 'policy_output': test_policy_targets},
                                    verbose=0
                                    )
                            print(f"Test losses: {losses}")

                            predictions = internal_model.predict(test_inputs, verbose=0)
                            value_preds = predictions[0].flatten()
                            avg_abs_value = np.mean(np.abs(value_preds))
                            print(f"Test value stats - Mean abs: {avg_abs_value:.4f}, Range: {np.min(value_preds):.4f} to {np.max(value_preds):.4f}")

                            plot_value_distribution(value_preds, f'test_values_{train_counter}.png')

                        send_ack(conn)
                        print("Training cycle completed successfully")

                    elif request_type == 'trainTD':
                        batch = 1

                        raw_inputs = receive_data(conn)
                        if raw_inputs is None:
                            print("Incomplete training inputs")
                            break
                        send_ack(conn)

                        raw_outputs = receive_data(conn)
                        if raw_outputs is None:
                            print("Incomplete training outputs")
                            break
                        send_ack(conn)

                        raw_targets = receive_data(conn)
                        if raw_targets is None:
                            print("Incomplete training targets")
                            break

                        targets = np.frombuffer(raw_targets, dtype=np.float64).reshape(batch, TOTAL_OUTPUT)
                        inputs = np.frombuffer(raw_inputs, dtype=np.float64).reshape(batch, TOTAL_INPUT)

                        value_targets = targets[:, 0:1]

                        # Only train the value head - keep existing policy predictions
                        policy_outputs = internal_model.predict(inputs, verbose=0)[1]

                        internal_model.train_on_batch(
                                x=inputs, 
                                y={'value_output': value_targets, 'policy_output': policy_outputs},
                                )

                        inverse_inputs = -inputs
                        inverse_value_targets = -value_targets

                        internal_model.train_on_batch(
                                x=inverse_inputs, 
                                y={'value_output': inverse_value_targets, 'policy_output': policy_outputs},
                                )

                        send_ack(conn)
                        print("TD Training cycle completed successfully")

                    else:
                        print(f"Unknown request: {request_type}")
                        break

                print("Connection closed, saving model...")
                try:
                    internal_model.save('neurelnet_internal.h5')
                    tf.keras.models.save_model(combined_model, 'neurelnet_combined.h5')
                    coreml_model = ct.convert(
                            combined_model,
                            inputs=[ct.TensorType(shape=(1, TOTAL_INPUT))],
                            )
                    coreml_model.save('neurelnet.mlpackage')
                except Exception as e:
                    print(f"Error saving model: {e}")
                print("Waiting for next client...")

if __name__ == '__main__':
    main()
