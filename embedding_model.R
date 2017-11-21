# train word embeddings on our abstract data!
library(purrr)
library(tidyverse)
library(stringr)
library(keras)

source('helper_functions.R')

###### Constants (writen in caps to make apparent)
NUM_ABSTRACTS <- 1000
CONTEXT_SIZE <- 2
HIDDEN_DIM <- 128
NUM_EPOCHS <- 50
EMBEDDING_DIM <- 30

###### Gather abstracts
abstracts_sample <- readRDS("bioarxiv_abstracts.rds") %>%
  # sample_n(NUM_ABSTRACTS) %>% 
# abstracts_sample <- readRDS("abstracts_small.rds") %>%
  .$abstract %>%
  sanitize_text()

split_abstracts <- abstracts_sample %>% 
  map(~str_split(., "\\s+")) %>% 
  unlist() %>% 
  {data_frame(word = .)} %>% 
  group_by(word) %>% 
  summarise(times = n())

# all the unique words in the abstracts
unique_words <- unique_vals(abstracts_sample, " ")

# How many unique words do we have?
vocab_size <- length(unique_words)

# transform each abstract to a sequence of integers.
text_encoder <- one_hot_encoder(unique_words, split = " ")

int_abstracts <- abstracts_sample %>%
  map(text_encoder$chr_2_int)

total_length <- (int_abstracts %>% map_int(length) %>% sum()) - 2*CONTEXT_SIZE

X <- array(0, dim = c(total_length, 4))
y <- array(0, dim = c(total_length, vocab_size))

step <- 1
for(abstract in int_abstracts){
  for(i in 3:(length(abstract)-2)){
    X[step,] <- c(abstract[i-2], abstract[i-1], abstract[i+1], abstract[i+2])
    y[step,] <- index_to_onehot(abstract[i], vocab_size)
    step <- step+1
  }
}

# saveRDS(X, "abstract_embed_X.rds")
# saveRDS(y, "abstract_embed_y.rds")
# 
# X <- readRDS("abstract_embed_X.rds")
# y <- readRDS("abstract_embed_y.rds")

rm()
gc()

model <- keras_model_sequential()
model %>% 
  layer_embedding(
    input_dim = vocab_size + 1, 
    output_dim = EMBEDDING_DIM, 
    input_length = 2*CONTEXT_SIZE
  ) %>% 
  layer_batch_normalization() %>% 
  layer_flatten() %>% 
  layer_dense(
    units = HIDDEN_DIM, 
    kernel_regularizer = regularizer_l2(l = 0.1),
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
  batch_size = 500,
  validation_split = 0.1,
  epochs = NUM_EPOCHS
)

save_model_hdf5(model, "embeddings_model2.h5")
 
which(unique_words == "methyladenosine")
test_x <- text_encoder$chr_2_int(" virus host cell")
unique_words[which.max(predict(model, t(test_x)))] 

embedding_weights <- model$layers[[1]]$get_weights()[[1]]
embedding_weights %>% dim()

embedding_results <- list(
  weights = embedding_weights,
  labels = unique_words
)

saveRDS(embedding_results, 'embedding_weights.rds')
