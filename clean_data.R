# train word embeddings on our abstract data!
library(purrr)
library(tidyverse)
library(stringr)

source('helper_functions.R')

###### Constants (writen in caps to make apparent)
NUM_ABSTRACTS <- 4000
CONTEXT_SIZE <- 2


if(NUM_ABSTRACTS == 4000){
  abstracts_sample <- readRDS('data/abstracts_sample_4000.rds')
} else {
  abstracts_sample <- readRDS("data/bioarxiv_abstracts.rds") %>%
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

# save pertinant info from this run for use in fitting and testing scripts
saveRDS(X, 'data/abstracts_X.rds')
saveRDS(y, 'data/abstracts_y.rds')
saveRDS(encoder_funcs, 'data/encoder_funcs.rds')
