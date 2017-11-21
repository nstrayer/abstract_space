library(purrr)
library(tidyverse)
library(stringr)
library(keras)

source('helper_functions.R')

###### Constants (writen in caps to make apparent)
SQUEEZE_DIM <- 256
MAX_LENGTH <- 256


###### Gather abstracts
abstracts_sample <- readRDS("bioarxiv_abstracts.rds") %>% 
  .$abstract %>% 
  sanitize_text()

NUM_ABSTRACTS <- length(abstracts_sample)

# all the unique words in the abstracts
unique_chars <- unique_vals(abstracts_sample, "")

# How many unique words do we have?
vocab_size <- length(unique_chars)

# What's the longest abstract we have?
# MAX_LENGTH <- abstracts_sample %>% 
#   find_MAX_LENGTH("")

# Padd all abstracts so that they are the same length. 
padded_abstracts <- abstracts_sample %>% 
  str_trunc(width = MAX_LENGTH, ellipsis = "")
# str_pad(width = MAX_LENGTH, side = "right")

train_test_data <- train_test_split(padded_abstracts, 0.9)

train_abstracts <- train_test_data$train
num_train <- length(train_abstracts)
test_abstracts <- train_test_data$test

# setup our encoder
oh_encoder <- one_hot_encoder(unique_chars)

## Set up training data: 
# map abstracts to integer encoding
train_int <- train_abstracts %>% 
  map(oh_encoder$chr_2_int)

train_oh <- train_abstracts %>% 
  map(oh_encoder$chr_2_oh)

# Vectorization
X <- array(0, dim = c(num_train, MAX_LENGTH))
y <- array(0, dim = c(num_train, MAX_LENGTH, vocab_size))

for(i in 1:num_train){
  X[i,] <- train_int[[i]]
  y[i,,] <- train_oh[[i]]
}

embedding_dim <- vocab_size + 1

# Initialize sequential model
model <- keras_model_sequential() 

model %>%
  layer_embedding(
    input_dim = vocab_size + 1, 
    output_dim = embedding_dim,
    input_length = MAX_LENGTH) %>% 
  layer_lstm(SQUEEZE_DIM, activation = "relu") %>%
  layer_repeat_vector(MAX_LENGTH) %>% 
  layer_lstm(SQUEEZE_DIM, activation = "relu", return_sequences = TRUE) %>% 
  time_distributed(
    layer_dense(units = vocab_size, activation = "softmax")
  )

# Compiling the model
model %>% compile(
  loss = "categorical_crossentropy", 
  optimizer = "adagrad", 
  metrics = "accuracy"
)

# Get the model summary
summary(model)

# Fitting loop
model %>% fit( 
  x = X, 
  y = y, 
  epochs = 5,
  batch_size = 200,
  validation_split = 0.1
)

save_model_hdf5(model, "250_char_abstract_encoder.h5")

# select a random abstract input
pred <- predict(model, t(X[34,]))[1,,]
pred %>% t() %>% 
  oh_encoder$oh_2_chr()
