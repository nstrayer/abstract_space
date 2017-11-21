sanitize_text <- . %>% 
  str_replace_all("\n", "") %>% 
  str_replace_all("[^[:alnum:]]", " ") %>% 
  tolower()

make_split <- function(word){
  if(word){
    split <- "\\s+"
  } else {
    split <- ""
  }
}
find_max_length <- function(char_vec, word = TRUE){
  split <- make_split(word)
  char_vec %>% 
    str_count(split) %>% 
    max()
}

# convert a given integer to a one-hot-encoding vector
index_to_onehot <- function(index, vocab_size){
  empty <- array(0, dim = c(vocab_size))
  empty[index] <- 1
  empty
}

train_test_split <- function(vec, train_perc){
  num_samples <- length(vec)
  num_train <- round(num_samples*train_perc)
  train_inds <- sample(1:num_train, replace = FALSE)
  list(
    train = vec[train_inds],
    test = vec[-train_inds]
  )
}

unique_vals <- function(char_vec, word = TRUE){
  split <- make_split(word)
  
  char_vec %>% 
    paste(collapse = split) %>% 
    strsplit(split) %>% 
    .[[1]] %>% 
    unique()
}


word_encoder <- function(text_array, cutoff = 25){
  word_frequencies <- text_array %>% 
    map(~str_split(., "\\s+")) %>% 
    unlist() %>% 
    data_frame(word = .) %>% 
    group_by(word) %>% 
    summarise(times = n()) %>% 
    filter(times > cutoff) %>% 
    mutate(integerVal = 1:n())
  
  # all the unique words in the abstracts
  unique_words <- word_frequencies$word
  
  # How many unique words do we have?
  # we add one here to account for our dummy holder 0
  vocab_size <- length(unique_words) + 1
  
  char_to_ints <- function(text){
    str_split(text, "\\s+")[[1]] %>% 
      as_data_frame() %>% 
      left_join( word_frequencies, by = c("value" = "word")) %>% {
        ifelse(is.na(.$integerVal), 0, .$integerVal)
      }
  }
  
  ints_to_char <- function(int_vec, merge = FALSE){
    data_frame(integerVal = int_vec) %>% 
      left_join(word_frequencies, by = "integerVal") %>% {
        ifelse(is.na(.$word), "_", .$word)
      }
  }
  
  # convert a given integer to a one-hot-encoding vector
  # we shift up the indexes by one to allow for our empty value 0.
  int_to_onehot <- function(int){
    empty <- array(0, dim = vocab_size)
    empty[int + 1] <- 1
    empty
  }
  
  list(
    unique_words = unique_words,
    vocab_size = vocab_size, 
    chr_to_int = char_to_ints,
    int_to_char = ints_to_char, 
    int_to_oh = int_to_onehot
  )
}


one_hot_encoder <- function(possible_vals, word = TRUE){
  split <- make_split(word)
  # convert a given integer to a one-hot-encoding vector
  index_to_onehot <- function(index, vocab_size){
    empty <- array(0, dim = c(vocab_size))
    empty[index] <- 1
    empty
  }
  
  # convert a string to a vector of integers
  chars_to_onehot <- function(text){
    str_split(text,split)[[1]] %>% 
      map_int(~which(possible_vals == .)) %>% 
      sapply(function(index){index_to_onehot(index, length(possible_vals))})
  }
  
  chars_to_int <- function(text){
    str_split(text, split)[[1]] %>% 
      map_int(~which(possible_vals == .)) 
  }
  
  # convert a vector of one-hot encodings back to a string
  one_hot_to_char <- function(oh_vec){
    oh_vec %>%
      apply(2, function(oh_array){
        possible_vals[which(oh_array == max(oh_array))]
      }) %>%
      paste(collapse = split)
  }
  
  int_to_char <- function(int_vec){
    int_vec %>%
      map_chr(~which(possible_vals == .)) %>%
      paste(collapse = split)
  }
  
  return(list(
    chr_2_oh = chars_to_onehot,
    chr_2_int = chars_to_int,
    oh_2_chr = one_hot_to_char,
    int_2_chr = int_to_char
  ))
  
}