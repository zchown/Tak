import tensorflow as tf
from tensorflow.keras.layers import (
        Input, Reshape, Conv2D, BatchNormalization, LeakyReLU,
        Add, Dropout, Flatten, Dense, Layer
        )
from tensorflow.keras.models import Model
import visualkeras

TOTAL_INPUT = 6 * 6 * 7
ROW_SIZE = 6
INPUT_SQUARE_DEPTH = 7
TOTAL_OUTPUT = 66

class Residual(Layer):

    def __init__(self, filters, dropout_rate=0.3, **kwargs):
        super(Residual, self).__init__(**kwargs)
        self.filters = filters
        self.dropout_rate = dropout_rate

        self.conv1 = Conv2D(filters, (3, 3), padding='same')
        self.bn1 = BatchNormalization()
        self.leaky1 = LeakyReLU(alpha=0.1)

        self.conv2 = Conv2D(filters, (3, 3), padding='same')
        self.bn2 = BatchNormalization()
        self.leaky2 = LeakyReLU(alpha=0.1)

        self.add = Add()
        self.leaky_out = LeakyReLU(alpha=0.1)
        self.dropout = Dropout(dropout_rate) if dropout_rate > 0 else None

    def call(self, inputs, training=None):
        shortcut = inputs

        x = self.conv1(inputs)
        x = self.bn1(x, training=training)
        x = self.leaky1(x)

        x = self.conv2(x)
        x = self.bn2(x, training=training)
        x = self.leaky2(x)

        x = self.add([x, shortcut])
        x = self.leaky_out(x)

        if self.dropout is not None:
            x = self.dropout(x, training=training)

        return x

    def get_config(self):
        config = super(Residual, self).get_config()
        config.update({
            'filters': self.filters,
            'dropout_rate': self.dropout_rate
            })
        return config

def create_unified_model():
    inp = Input(shape=(TOTAL_INPUT,), name='input')
    x = Reshape((ROW_SIZE, ROW_SIZE, INPUT_SQUARE_DEPTH), name='reshape')(inp)

    x = Conv2D(128, (3, 3), activation='sigmoid', padding='same', name='entry_conv')(x)
    x = BatchNormalization(name='entry_bn')(x)
    x = LeakyReLU(alpha=0.1, name='entry_leaky')(x)

    for i in range(4):
        x = Residual(128, name=f'residual_block_{i}a')(x)
        x = Residual(128, name=f'residual_block_{i}b')(x)

    x = Conv2D(16, (1, 1), activation='sigmoid', padding='same', name='bottleneck_conv')(x)
    x = BatchNormalization(name='bottleneck_bn')(x)
    x = LeakyReLU(alpha=0.1, name=f"bottleneck_leaky")(x)

    x = Flatten(name='flatten')(x)
    out = Dense(TOTAL_OUTPUT, activation='tanh', name='combined_output')(x)

    model = Model(inputs=inp, outputs=out, name='UnifiedResNet')
    model.compile(
            optimizer=tf.keras.optimizers.legacy.Adam(learning_rate=0.01),
            loss='mean_squared_error',
            metrics=['mean_squared_error']
            )
    return model

def visualize_model():
    """Create visualizations of the model architecture"""
    model = create_unified_model()
    model.summary()

    try:
        layered_view = visualkeras.layered_view(
                model,
                to_file='model_layered.png',
                legend=True,
                )
        print("Created model_layered.png")
    except Exception as e:
        print(f"Error creating layered view: {e}")
        try:
            layered_view = visualkeras.layered_view(model, to_file='model_layered_simple.png')
            print("Created simplified model_layered_simple.png")
        except Exception as e2:
            print(f"Error creating simplified layered view: {e2}")

    try:
        from tensorflow.keras.utils import plot_model
        plot_model(
                model, 
                to_file='model_architecture.png',
                show_shapes=True,
                show_layer_names=True,
                rankdir='TB',
                dpi=96
                )
        print("Created model_architecture.png")
    except Exception as e:
        print(f"Error creating architecture diagram: {e}")

    try:
        graph_view = visualkeras.graph_view(
                model,
                to_file='model_graph.png',
                )
        print("Created model_graph.png")
    except Exception as e:
        print(f"Error creating graph view: {e}")

    print("\nModel visualization completed. Check the output files.")

if __name__ == '__main__':
    visualize_model()
