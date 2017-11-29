# train word embeddings on our abstract data!
library(purrr)
library(tidyverse)
library(stringr)
library(keras)

source('helper_functions.R')

###### Constants (writen in caps to make apparent)
NUM_ABSTRACTS <- 4000
CONTEXT_SIZE <- 2
HIDDEN_DIM <- 256
NUM_EPOCHS <- 400
EMBEDDING_DIM <- 32


if(NUM_ABSTRACTS == 4000){
  abstracts_sample <- readRDS('data/abstracts_sample_4000.rds')
} else {
  abstracts_sample <- readRDS("bioarxiv_abstracts.rds") %>%
    sample_n(NUM_ABSTRACTS) %>%
    .$abstract %>%
    sanitize_text() %>%
    .[str_count(.," ") > 10] #make sure abstracts are at least 10 words long
}

NUM_ABSTRACTS <- length(abstracts_sample)

# Set up custom encoding functions.
encoder_funcs <- word_encoder(abstracts_sample, cutoff = 50)
# encoder_funcs$chr_to_int(abstracts_sample[1])

# all the unique words in the abstracts
unique_words <- encoder_funcs$unique_words

# How many unique words do we have? (this includes the 0 or misc category in it)
vocab_size <- encoder_funcs$vocab_size

# Convert our abstract character strings to an array of integers
int_abstracts <- abstracts_sample %>%
  map(encoder_funcs$chr_to_int)

total_length <- int_abstracts %>% 
  map_int(length) %>% 
  {. - 2*CONTEXT_SIZE} %>% 
  sum()

X <- array(0, dim = c(total_length, 4))
y <- array(0, dim = c(total_length, vocab_size))

step <- 1
for(abstract in int_abstracts){
  for(i in 3:(length(abstract)-2)){
    X[step,] <- c(abstract[i-2], abstract[i-1], abstract[i+1], abstract[i+2])
    y[step,] <- encoder_funcs$int_to_oh(abstract[i])
    step <- step+1
  }
}

rm()
gc()

model <- keras_model_sequential()
model %>% 
  layer_embedding(
    input_dim = vocab_size, 
    output_dim = EMBEDDING_DIM, 
    input_length = 2*CONTEXT_SIZE,
    mask_zero = F
  ) %>% 
  layer_batch_normalization() %>% 
  layer_flatten() %>%
  layer_dense(
    units = HIDDEN_DIM, 
    kernel_regularizer = regularizer_l2(l = 0.05),
    activation = 'relu') %>% 
  layer_batch_normalization() %>% 
  layer_dense(
    units = vocab_size, 
    kernel_regularizer = regularizer_l2(l = 0.1),
    activation = 'softmax'
  ) 

model %>% compile(
  loss = "categorical_crossentropy",
  optimizer = "adagrad",
  metrics = "accuracy"
)

model %>% fit(
  X, y, 
  batch_size = 2500,
  validation_split = 0.1,
  epochs = NUM_EPOCHS
)

save_model_hdf5(model, "embeddings_model2.h5")
 
model <- load_model_hdf5("embeddings_model2.h5")
test_x <- encoder_funcs$chr_to_int("rna sequence for data")
unique_words[which.max(predict(model, t(test_x)))] 

embedding_weights <- model$layers[[1]]$get_weights()[[1]]
embedding_weights %>% dim()

embedding_results <- list(
  weights = embedding_weights,
  labels = unique_words
)

saveRDS(embedding_results, 'embedding_weights.rds')
