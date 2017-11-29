# visualizing the word embeddings obtained from abstract scraping. 

# install.packages("plotly")
# install.packages(c("Rtsne", "plotly"))
library(Rtsne)
library(plotly)


embedding_data <- readRDS("data/embedding_weights.rds")
X <- embedding_data$weights
labels <- embedding_data$labels
dim(X)
length(labels)


tsne <- Rtsne(X, dims = 2, perplexity=15, verbose=TRUE, max_iter = 4000)

tsne_plot <- tsne$Y %>% 
  as_data_frame() %>% 
  tail(-1) %>% 
  mutate(label = labels) %>% 
  ggplot(aes(x = V1, y = V2, label = label)) +
  geom_point(alpha = 0.6, color = "steelblue") +
  theme_minimal()

ggplotly(tsne_plot)
