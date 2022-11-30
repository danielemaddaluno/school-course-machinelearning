## https://tensorflow.rstudio.com/guide/keras/
# install.packages("keras")
# install_keras()
## Installare tensorFlow
## https://www.tensorflow.org/install/pip#3.-install-the-tensorflow-pip-package

library(keras)
mnist <- dataset_mnist()
x_train <- mnist$train$x
y_train <- mnist$train$y
x_test <- mnist$test$x
y_test <- mnist$test$y

# # reshape
# x_train <- array_reshape(x_train, c(nrow(x_train), 784))
# x_test <- array_reshape(x_test, c(nrow(x_test), 784))
# rescale
x_train <- x_train / 255
x_test <- x_test / 255


y_train <- to_categorical(y_train, 10)
y_test <- to_categorical(y_test, 10)

model <- keras_model_sequential() 
# model %>% 
#   layer_dense(units = 256, activation = 'relu', input_shape = c(784)) %>% 
#   layer_dropout(rate = 0.4) %>% 
#   layer_dense(units = 128, activation = 'relu') %>%
#   layer_dropout(rate = 0.3) %>%
#   layer_dense(units = 10, activation = 'softmax')

## Python code
# # Define the model architecture
# model = keras.Sequential([
#   keras.layers.Flatten(input_shape=(28, 28)),
#   keras.layers.Dense(128, activation=tf.nn.relu),
#   
#   # Optional: You can replace the dense layer above with the convolution layers below to get higher accuracy.
#   # keras.layers.Reshape(target_shape=(28, 28, 1)),
#   # keras.layers.Conv2D(filters=32, kernel_size=(3, 3), activation=tf.nn.relu),
#   # keras.layers.Conv2D(filters=64, kernel_size=(3, 3), activation=tf.nn.relu),
#   # keras.layers.MaxPooling2D(pool_size=(2, 2)),
#   # keras.layers.Dropout(0.25),
#   # keras.layers.Flatten(input_shape=(28, 28)),
#   # keras.layers.Dense(128, activation=tf.nn.relu),
#   # keras.layers.Dropout(0.5),
#   
#   keras.layers.Dense(10)
# ])

model %>%
  layer_flatten(input_shape=c(28, 28)) %>%

  layer_reshape(target_shape=c(28, 28, 1)) %>%
  layer_conv_2d(filters=32, kernel_size=c(3, 3), activation='relu') %>%
  layer_conv_2d(filters=64, kernel_size=c(3, 3), activation='relu') %>%
  layer_max_pooling_2d(pool_size=c(2, 2)) %>%
  layer_dropout(0.25) %>%
  layer_flatten(input_shape=c(28, 28)) %>%
  layer_dense(units = 128, activation = 'relu') %>%
  layer_dropout(0.5) %>%
  
  layer_dense(units = 10, activation = 'softmax')


summary(model)


model %>% compile(
  loss = 'categorical_crossentropy',
  optimizer = optimizer_adam(), #optimizer_rmsprop(),
  metrics = c('accuracy')
)

history <- model %>% fit(
  x_train, y_train, 
  epochs = 30, batch_size = 128, 
  validation_split = 0.2
)

plot(history)

model %>% evaluate(x_test, y_test)
model %>% predict_classes(x_test)



#####################################
### Save the model for later use: ###
#####################################

# https://tensorflow.rstudio.com/guide/keras/guide_keras/
# Save entire model to the SavedModel format
model %>% save_model_tf('model_keras/')


library("clipr")
# Convert Keras model in TensorFlow Lite model
# https://tensorflow.rstudio.com/guide/tfhub/hub-with-keras/
# https://www.tensorflow.org/lite/convert/#converting_a_savedmodel_
print("Per convertire il modello appena salvato in un modello per tensorflow_lite esegui questo da linea di comando:")
cmdToExecute = paste("tflite_convert", " ",
                     "--saved_model_dir=", 
                     getwd(), "/", "model_keras", " ", 
                     "--output_file=",
                     getwd(), "/", "mnist.tflite", sep="")
cmdToExecute
write_clip(cmdToExecute) # copia automaticamente nella clipboard il test da incollare nella cmd

## Conversione di un SavedModel
# tflite_convert \
# --saved_model_dir=/tmp/mobilenet_saved_model \
# --output_file=/tmp/mobilenet.tflite

## Conversione di un modello Keras H5 
# tflite_convert \
# --keras_model_file=/tmp/mobilenet_keras_model.h5 \
# --output_file=/tmp/mobilenet.tflite