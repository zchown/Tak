import socket
import coremltools as ct
import struct
import numpy as np
import tensorflow as tf
from tensorflow.keras.models import Model
from tensorflow.keras.layers import Dense, Flatten, Reshape, Input, Conv3D, Conv2D, MaxPooling2D
from tensorflow.keras.layers import MaxPooling3D, Dropout, BatchNormalization, Concatenate
import random
from collections import deque

# Model configuration
TOTAL_INPUT = 6 * 6 * 7 * 3
INPUT_SQUARES = 36
ROW_SIZE = 6
INPUT_SQUARE_DEPTH = 7
INPUT_PIECE_TYPES = 3
POLICY_SIZE = 65
TOTAL_OUTPUT = 66

# Experience replay buffer
class ExperienceBuffer:
    def __init__(self, max_size=5000):
        self.buffer = deque(maxlen=max_size)

    def add(self, inputs, targets):
        # if (targets[0][0] < 0.5) and (targets[0][0] > -0.5):
            # return
        self.buffer.append((inputs, targets))

        inverse_inputs = -inputs
        inverse_targets = targets.copy()
        inverse_targets[0] = -targets[0]

        self.buffer.append((inverse_inputs, inverse_targets))

    def sample(self, batch_size):
        if batch_size > len(self.buffer):
            batch_size = len(self.buffer)

        batch = random.sample(self.buffer, batch_size)
        inputs = np.vstack([item[0] for item in batch])
        targets = np.vstack([item[1] for item in batch])
        return inputs, targets

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

def create_alphazero_model():
    input_layer = Input(shape=(TOTAL_INPUT,), name='input')
    x = Reshape((ROW_SIZE, ROW_SIZE, INPUT_SQUARE_DEPTH, INPUT_PIECE_TYPES))(input_layer)
    
    x = Conv3D(256, (3, 3, 7), activation='relu', padding='same', kernel_regularizer=tf.keras.regularizers.l2(0.001))(x)
    x = MaxPooling3D(pool_size=(1, 1, INPUT_SQUARE_DEPTH))(x)
    x = Reshape((ROW_SIZE, ROW_SIZE, 256))(x)
    x = BatchNormalization()(x)
    x = Dropout(0.25)(x)
    
    x = Conv2D(64, (3, 3), activation='relu', padding='same', kernel_regularizer=tf.keras.regularizers.l2(0.001))(x)
    x = MaxPooling2D(pool_size=(3, 3))(x)
    x = BatchNormalization()(x)
    x = Dropout(0.25)(x)
    
    x = Conv2D(32, (3, 3), activation='relu', padding='same', kernel_regularizer=tf.keras.regularizers.l2(0.01))(x)
    # x = MaxPooling2D(pool_size=(3, 3))(x)
    x = BatchNormalization()(x)
    x = Dropout(0.25)(x)
    #
    # x = Conv2D(256, (3, 3), activation='relu', padding='same', kernel_regularizer=tf.keras.regularizers.l2(0.01))(x)
    # x = BatchNormalization()(x)
    # x = Dropout(0.25)(x)
    # x = Conv2D(64, (3, 3), activation='relu', padding='same', kernel_regularizer=tf.keras.regularizers.l2(0.01))(x)
    # x = BatchNormalization()(x)

    x = Flatten()(x)
    
    # x = Dense(1024, activation='relu')(x)
    # x = BatchNormalization()(x)
    # x = Dense(1024, activation='relu')(x)
    # x = Dropout(0.25)(x)
    x = BatchNormalization()(x)
    x = Dense(512, activation='relu')(x)
    dropout = Dropout(0.5)(x)
    # x = Dense(512, activation='relu')(x)
    x = BatchNormalization()(x)
    x = Dense(256, activation='relu')(x)
    x = BatchNormalization()(x)

    
    shared_output = Dense(256, activation='relu')(x)
    shared_output = BatchNormalization()(shared_output)
    
    # Policy head (output move probabilities)
    policy_hidden = Dense(128, activation='relu')(shared_output)
    policy_output = Dense(POLICY_SIZE, activation='softmax', name='policy_output')(policy_hidden)
    
    # Value head (output position evaluation)
    value_hidden = Dense(128, activation='relu')(shared_output)
    value_hidden = BatchNormalization()(value_hidden)
    value_output = Dense(1, activation='tanh', name='value_output')(value_hidden)  # tanh for [-1,1] range
    
    # Create internal model with two output heads for training
    internal_model = Model(inputs=input_layer, outputs=[value_output, policy_output])
    
    # Wrapper for CoreML conversion
    combined_output = Concatenate(name='combined_output')([value_output, policy_output])
    combined_model = Model(inputs=input_layer, outputs=combined_output)
    
    # Compile the internal model for training
    internal_model.compile(
        optimizer=tf.keras.optimizers.legacy.Adam(learning_rate=0.0001),
        loss={
            'value_output': 'mean_squared_error',
            'policy_output': 'categorical_crossentropy'
        },
        loss_weights={
            'value_output': 2.0,
            'policy_output': 1.0
        },
        metrics={
            'value_output': 'mean_squared_error',
            'policy_output': 'accuracy'
        }
    )

    # print model summary
    internal_model.summary()
    combined_model.summary()
    
    return internal_model, combined_model

def main():
    internal_model, combined_model = create_alphazero_model()
    print("AlphaZero-style model ready, listening for connections...")

    experience_buffer = ExperienceBuffer(max_size=10000)
    replay_batch_size = 32
    train_counter = 0
    replay_frequency = 5

    try:
        internal_model = tf.keras.models.load_model('neurelnet_internal.h5')
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

                        print(f"Value prediction: {combined_output[0][0]}")

                        prediction_bytes = struct.pack('!66f', *combined_output[0])
                        conn.sendall(prediction_bytes)

                        if not wait_for_ack(conn):
                            print("Failed to receive ACK after sending prediction")
                            break

                        print("Prediction cycle completed successfully")

                    elif request_type == 'train':
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
                        # train on inverse
                        inverse_inputs = -inputs
                        inverse_targets = targets.copy()
                        inverse_targets[0] = -targets[0]
                        internal_model.train_on_batch(
                                x=inverse_inputs, 
                                y={'value_output': inverse_targets[:, 0:1], 'policy_output': inverse_targets[:, 1:TOTAL_OUTPUT]}
                                )

                        train_counter += 1
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

                        # Experience replay
                        if train_counter % replay_frequency == 0 and len(experience_buffer.buffer) >= replay_batch_size * 10:
                            replay_inputs, replay_targets = experience_buffer.sample(replay_batch_size)

                            replay_value_targets = replay_targets[:, 0:1]
                            replay_policy_targets = replay_targets[:, 1:TOTAL_OUTPUT]

                            internal_model.train_on_batch(
                                    x=replay_inputs, 
                                    y={'value_output': replay_value_targets, 'policy_output': replay_policy_targets}
                                    )

                        if train_counter % 1000 == 0:
                            # run a bunch of batches and also run tests to see if the model is learning
                            for _ in range(100):
                                replay_inputs, replay_targets = experience_buffer.sample(replay_batch_size)
                                replay_value_targets = replay_targets[:, 0:1]
                                replay_policy_targets = replay_targets[:, 1:TOTAL_OUTPUT]

                                internal_model.train_on_batch(
                                        x=replay_inputs, 
                                        y={'value_output': replay_value_targets, 'policy_output': replay_policy_targets}
                                        )
                            # run a test only on value head
                            test_inputs, test_targets = experience_buffer.sample(replay_batch_size)
                            test_value_targets = test_targets[:, 0:1]
                            test_policy_targets = test_targets[:, 1:TOTAL_OUTPUT]
                            losses = internal_model.evaluate(
                                    x=test_inputs, 
                                    y={'value_output': test_value_targets, 'policy_output': test_policy_targets},
                                    verbose=0
                                    )
                            print(f"Test losses: {losses}")

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

                        #only train the value head
                        policy_targets = internal_model.predict(inputs, verbose=0)[1]

                        internal_model.train_on_batch(
                                x=inputs, 
                                y={'value_output': value_targets, 'policy_output': policy_targets},
                                )

                        # train on inverse
                        inverse_inputs = -inputs
                        inverse_targets = targets.copy()
                        inverse_targets[0] = -targets[0]
                        internal_model.train_on_batch(
                                x=inverse_inputs, 
                                y={'value_output': inverse_targets[:, 0:1], 'policy_output': policy_targets},
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
