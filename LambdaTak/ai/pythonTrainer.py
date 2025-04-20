import socket
import struct
import numpy as np
import tensorflow as tf
from tensorflow.keras.models import Sequential
from tensorflow.keras.layers import Dense, Flatten, Conv2D, MaxPooling2D, Reshape, Input, Conv3D, MaxPooling3D

# Model configuration
TOTAL_INPUT = 6 * 6* 7 * 3
INPUT_SQUARES = 36
ROW_SIZE = 6
INPUT_SQUARE_DEPTH = 7
INPUT_PIECE_TYPES = 3

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
        Conv3D(64, (3, 3, 7), activation='relu', padding='same'),
        MaxPooling3D(pool_size=(2, 2, 1)),
        Conv3D(128, (3, 3, 7), activation='relu', padding='same'),
        Flatten(),
        Dense(128, activation='relu'),
        Dense(64, activation='relu'),
        Dense(1, activation='sigmoid', name='output')
        ])
    model.compile(optimizer='adam', loss='mse', metrics=['accuracy'])
    return model


def main():
    model = create_model()
    print("Model ready, listening for connections...")

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

                        prediction_bytes = struct.pack('!f', float(outputs[0][0]))

                        print(f"Sending bytes: {' '.join(f'{b:02x}' for b in prediction_bytes)}")

                        conn.sendall(prediction_bytes)

                        if not wait_for_ack(conn):
                            print("Failed to receive ACK after sending prediction")
                            break

                        print("Prediction cycle completed successfully")

                    elif request_type == 'train':
                        batch = 1

                        # Read raw inputs
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

                        targets = np.frombuffer(raw_targets, dtype=np.float64).reshape(batch, 1)
                        model.train_on_batch(inputs, targets)

                        send_ack(conn)

                        print("Training cycle completed successfully")

                    else:
                        print(f"Unknown request: {request_type}")
                        break

                print("Connection closed, saving model...")
                try:
                    model.save('neurelnet.h5')
                except Exception as e:
                    print(f"Error saving model: {e}")
                print("Waiting for next client...")

if __name__ == '__main__':
    main()
