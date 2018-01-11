library(tidyverse)
library(keras)
 
spell_data <- read.csv( 'spell_data.csv', stringsAsFactors = F) %>% 
  filter(label != 'Expeliarmus')
unique_spells <- unique(spell_data$label)

wide_data <- spell_data %>% 
  spread(direction, accel)

gestures_vecs <- wide_data %>% 
  group_by(label, recording_num) %>% 
  do(
    data = .[c('m_x','m_y','m_z')] %>% 
      as.matrix()
  )


X <- gestures_vecs$data %>% 
  pad_sequences()

y <- wide_data %>% 
  group_by(label, recording_num) %>% 
  summarise(
    spell = first(label)
  ) %>% {
    map_int(.$spell, ~(which(unique_spells==.)))
  } %>% {
    . - 1
  } %>% 
  to_categorical() 

num_obs <- dim(y)[1]

# shuffle data so that keras pulls off a good validation set. 
shuffled_indices <- 1:num_obs %>% sample()

X <- X[shuffled_indices, , ]
y <- y[shuffled_indices, ]


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
    epochs = 100,
    validation_split = 0.35
  )

model %>% save_model_hdf5('cast_spells/spell_model.h5')
unique_spells %>% saveRDS('cast_spells/spell_list.rds')
