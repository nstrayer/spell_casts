library(tidyverse)
library(keras)
 
spell_data <- read.csv( 'spell_data.csv', stringsAsFactors = F)

spell_data  %>% 
  ggplot(aes(x = time, y= accel, color = direction)) +
  geom_line() + 
  facet_wrap(~id) +
  theme_void()
  
gestures_vecs <- spell_data %>% 
  spread(direction, accel) %>%
  group_by(id,spell) %>% 
  do(
    data = .[c('x','y','z')] %>% as.matrix()
  )

unique_spells <- unique(gestures_vecs$spell)

X <- gestures_vecs$data %>% 
  pad_sequences()

y <- gestures_vecs$spell %>% 
  map_int(~(which(unique_spells==.))) %>% 
  to_categorical() %>% 
  .[,-1] #gets rid of the zero column left over from python stuffs

dim(X)
dim(y)

filters <- 15
kernel_size <- 10
hidden_dims <- 16
input_shape <- dim(X)[-1]
num_classes <- dim(y)[2]

#Initialize model
model <- keras_model_sequential()
model %>% 
  layer_conv_1d(
    filters = filters,
    kernel_size = kernel_size, 
    input_shape = input_shape,
    strides = 1,
    padding = "valid", 
    activation = "relu",
    name = 'convolution'
  ) %>%
  layer_batch_normalization() %>% 
  # Apply max pooling:
  layer_global_max_pooling_1d(name = 'max_pooling') %>%
  # Project onto a single unit output layer, and squash it with a sigmoid
  layer_dropout(0.4) %>% 
  layer_dense(
    num_classes, 
    activation = 'softmax',
    name = 'dense_output'
  ) 

model %>% summary()

# Compile model
model %>% compile(
  loss = "categorical_crossentropy",
  optimizer = "rmsprop",
  metrics = "accuracy"
)

model %>%
  fit(
    X, y,
    epochs = 250,
    validation_split = 0.35
  )

model %>% save_model_hdf5('spell_model.h5')
