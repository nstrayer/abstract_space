library(keras)
library(tidyverse)

model <- load_model_hdf5("fit_models/embeddings_model2.h5")

test_x <- encoder_funcs$chr_to_int("rna sequence for data")
unique_words[which.max(predict(model, t(test_x)))] 

embedding_weights <- model$layers[[1]]$get_weights()[[1]]
embedding_weights %>% dim()

embedding_results <- list(
  weights = embedding_weights,
  labels = unique_words
)

saveRDS(embedding_results, 'embedding_weights.rds')
