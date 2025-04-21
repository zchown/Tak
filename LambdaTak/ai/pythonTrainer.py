import socket
import coremltools as ct
import struct
import coremltools as ct
import numpy as np
import tensorflow as tf
from tensorflow.keras.models import Sequential
from tensorflow.keras.layers import Dense, Flatten, Reshape, Input, Conv3D
from tensorflow.keras.layers import MaxPooling3D, Dropout, BatchNormalization
import random
from collections import deque

# Model configuration
TOTAL_INPUT = 6 * 6 * 7 * 3
INPUT_SQUARES = 36
ROW_SIZE = 6
INPUT_SQUARE_DEPTH = 7
INPUT_PIECE_TYPES = 3

# Experience replay buffer
class ExperienceBuffer:
    def __init__(self, max_size=1000):
        self.buffer = deque(maxlen=max_size)

    def add(self, inputs, targets):
        self.buffer.append((inputs, targets))

        # Add inverse example 
        inverse_inputs = -inputs
        inverse_targets = targets.copy()
        inverse_targets[0][0] = 1 - targets[0][0]
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
        print(f"Received {len(data)} bytes, waiting for {n} bytes")
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
    print("Sent ACK")


def wait_for_ack(conn):
    ack = recv_all(conn, 4)
    if ack is None:
        print("Connection closed while waiting for ACK")
        return False
    if ack != b'ACK\0':
        print(f"Expected ACK, got {ack}")
        return False
    print("Received ACK")
    return True


def create_model():
    model = Sequential([
        Input(shape=(TOTAL_INPUT,), name='input'),
        Reshape((ROW_SIZE, ROW_SIZE, INPUT_SQUARE_DEPTH, INPUT_PIECE_TYPES)),
        Conv3D(64, (3, 3, 7), activation='relu', padding='same', kernel_regularizer='l2'),
        Dropout(0.5),
        Dropout(0.5),
        Conv3D(128, (3, 3, 7), activation='relu', padding='same', kernel_regularizer='l2'),
        Dropout(0.5),
        MaxPooling3D(pool_size=(2, 2, 1)),
        Conv3D(128, (3, 3, 7), activation='relu', padding='same'),
        MaxPooling3D(pool_size=(2, 2, 1)),
        Dropout(0.5),
        Conv3D(256, (3, 3, 7), activation='relu', padding='same'),
        Flatten(),
        BatchNormalization(),
        Dense(512, activation='relu'),
        Dropout(0.25),
        Dense(256, activation='relu'),
        Dropout(0.25),
        Dense(128, activation='relu'),
        BatchNormalization(),
        Dense(66, activation='sigmoid', name='output')
        ])
    model.compile(optimizer='adam', loss='binary_crossentropy', metrics=['accuracy'])
    model.optimizer.learning_rate = 0.0001
    return model


def main():
    model = create_model()
    print("Model ready, listening for connections...")

    experience_buffer = ExperienceBuffer(max_size=10000)
    replay_batch_size = 64 
    train_counter = 0
    replay_frequency = 1

    try:
        model = tf.keras.models.load_model('neurelnet.h5')
        print("Loaded existing model")
        print("Converted and saved CoreML model")
    except (FileNotFoundError, OSError):
        print("No existing model found, using new model")
    except Exception as e:
        print(f"Error loading model: {e}")
        return
    coreml_model = ct.converters.convert(model, inputs=[ct.TensorType(shape=(1, TOTAL_INPUT))])
    coreml_model.save('neurelnet.mlpackage')

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
                        outputs = model.predict(inputs, verbose=0)
                        print(f"Prediction raw value: {outputs[0][0]}")

                        # send all 66 outputs
                        prediction_bytes = struct.pack('!66f', *outputs[0])

                        print(f"Sending bytes: {' '.join(f'{b:02x}' for b in prediction_bytes)}")

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

                        targets = np.frombuffer(raw_targets, dtype=np.float64).reshape(batch, 66)

                        experience_buffer.add(inputs, targets)

                        model.train_on_batch(inputs, targets)
                        inputsInverse = -inputs
                        targetsInverse = targets.copy()
                        targetsInverse[0][0] = 1 - targets[0][0]
                        model.train_on_batch(inputsInverse, targetsInverse)

                        train_counter += 1
                        if train_counter % 100 == 0:
                            #save model
                            try:
                                model.save('neurelnet.h5')
<<<<<<< HEAD
                                coreml_model = ct.convert(
                                    "neurelnet.h5",
                                    source='tensorflow',
                                    inputs=[ct.TensorType(shape=(1, TOTAL_INPUT))],
                                )
=======
                                coreml_model = ct.converters.convert(model, inputs=[ct.TensorType(shape=(1, TOTAL_INPUT))])
>>>>>>> policyNetworks
                                coreml_model.save('neurelnet.mlpackage')
                            except Exception as e:
                                print(f"Error saving model: {e}")
                        if train_counter % 10 == 0:
                            # Print distribution of predictions and targets
                            print(f"Target distribution: min={targets.min()}, max={targets.max()}, mean={targets.mean()}")
                            test_preds = model.predict(inputs)
                            print(f"Prediction distribution: min={test_preds.min()}, max={test_preds.max()}, mean={test_preds.mean()}")

                            # Check model weights for extreme values
                            for layer in model.layers:
                                if hasattr(layer, 'weights') and layer.weights:
                                    weights = layer.weights[0].numpy()
                                    print(f"Layer {layer.name} weights - min: {weights.min()}, max: {weights.max()}, mean: {weights.mean()}")

                        if train_counter % replay_frequency == 0 and len(experience_buffer.buffer) >= replay_batch_size:
                            replay_inputs, replay_targets = experience_buffer.sample(replay_batch_size)
                            print(f"Training on {replay_batch_size} samples from experience buffer")
                            model.train_on_batch(replay_inputs, replay_targets)

                        send_ack(conn)

                        print("Training cycle completed successfully")

                    else:
                        print(f"Unknown request: {request_type}")
                        break

                print("Connection closed, saving model...")
                try:
                    model.save('neurelnet.h5')
                    coreml_model = ct.convert(
                        "neurelnet.h5",
                        source='tensorflow',
                        inputs=[ct.TensorType(shape=(1, TOTAL_INPUT))],
                    )
                    coreml_model.save('neurelnet.mlpackage')
                except Exception as e:
                    print(f"Error saving model: {e}")
                print("Waiting for next client...")

if __name__ == '__main__':
    main()
