import coremltools as ct
import tensorflow as tf
from tensorflow import keras
from tensorflow.keras.models import Sequential
from tensorflow.keras.layers import Dense, Dropout, Flatten, Conv2D, MaxPooling2D, Reshape, Input, Conv3D

TOTAL_INPUT = 36 * 7 * 3
INPUT_SQUARES = 36
INPUT_SQUARE_DEPTH = 7
INPUT_PIECE_TYPES = 3

model = Sequential([
    Input(shape=(TOTAL_INPUT,), name='input'),
    Reshape((INPUT_SQUARES, INPUT_SQUARE_DEPTH, INPUT_PIECE_TYPES)),
    Conv2D(64, (3, 3), activation='relu'),
    Conv2D(128, (3, 3), activation='relu'),
    MaxPooling2D(pool_size=(2, 2)),
    Flatten(),
    Dense(128, activation='relu'),
    Dense(64, activation='relu'),
    Dense(1, activation='sigmoid', name='output')
])

model.compile(optimizer='adam', loss='binary_crossentropy', metrics=['accuracy'])

model.build((None, TOTAL_INPUT))
model.summary()

model.save('neurelnet.h5')


coreml_model = ct.convert(
    "neurelnet.h5",
    source='tensorflow',
    inputs=[ct.TensorType(shape=(1, TOTAL_INPUT))],
)

coreml_model.save('neurelnet.mlpackage')
