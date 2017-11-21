# visualizing the word embeddings obtained from abstract scraping. 

# install.packages("plotly")
library(Rtsne)
library(plotly)


embedding_data <- readRDS("embedding_weights.rds")
X <- embedding_data$weights
labels <- embedding_data$labels
dim(X)
length(labels)


tsne <- Rtsne(X, dims = 2, perplexity=20, verbose=TRUE, max_iter = 2000)

tsne_plot <- tsne$Y %>% 
  as_data_frame() %>% 
  tail(-1) %>% 
  mutate(label = labels) %>% 
  ggplot(aes(x = V1, y = V2, label = label)) +
  geom_point(alpha = 0.3, color = "steelblue") +
  theme_minimal()

ggplotly(tsne_plot)
