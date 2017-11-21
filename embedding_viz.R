# visualizing the word embeddings obtained from abstract scraping. 

install.packages("plotly")
library(Rtsne)
library(plotly)


embedding_data <- readRDS("embedding_weights.rds")

X <- embedding_data$weights
dim(X)
Rtsne

tsne <- Rtsne(X, dims = 2, perplexity=10, verbose=TRUE, max_iter = 2000)

tsne$Y %>% 
  as_data_frame() %>% 
  ggplot(aes(x = V1, y = V2)) +
  geom_point(alpha = 0.1) +
  theme_minimal()

ggplotly(tsne_plot)
plot(tsne$Y, t='n', main="tsne")
