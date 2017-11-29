# train word embeddings on our abstract data!
library(keras)

X <- readRDS('data/abstracts_X.rds')
y <- readRDS('data/abstracts_y.rds')

###### Constants (writen in caps to make apparent)
CONTEXT_SIZE <- 2
HIDDEN_DIM <- 256
NUM_EPOCHS <- 400
EMBEDDING_DIM <- 32
VOCAB_SIZE <- max(X) + 1

rm()
gc()

model <- keras_model_sequential()
model %>% 
  layer_embedding(
    input_dim = VOCAB_SIZE, 
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
    units = VOCAB_SIZE, 
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

save_model_hdf5(model, "fit_models/embeddings_model2.h5")

