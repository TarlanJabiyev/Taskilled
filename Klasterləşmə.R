library(tidyverse)
library(plotly)
library(factoextra) 
library(NbClust) 
library(cluster) 

data("iris")

iris %>% glimpse()

iris$Species %>% table()

df <- iris %>% select(-Species) %>% scale()


# Klasterlərin optimal sayını tapmaq ----

df %>% 
  fviz_nbclust(kmeans, method = "wss") +
  labs(subtitle = "Elbow method")
# "Elbow" metodu: 2 klaster həlli təklif olunur

df %>% 
  fviz_nbclust(kmeans, method = "silhouette") +
  labs(subtitle = "Silhouette method")
# "Silhouette" metodu: 2 klaster həlli təklif olunur

df %>% 
  fviz_nbclust(kmeans, method = "gap_stat")+
  labs(subtitle = "Gap statistic method")
# "Gap Statistic" metodu: 2 klaster həlli təklif olunur


# "K-Means" Klasterləşmə modeli ----
set.seed(123)
kmeans <- df %>% kmeans(centers = 2)
y_kmeans <- kmeans$cluster %>% as_factor()

# Nəticəni vizuallaşdırmaq
df %>% clusplot(y_kmeans,
                shade = T,
                color = T,
                labels = 2,
                plotchar = F,
                main = "Klasterlər")

g <- iris %>% 
  ggplot(aes(Sepal.Length,Petal.Length,
             color = y_kmeans)) +
  geom_point(aes(text = Species),size = 2) + 
  #facet_wrap(~ Species) +
  scale_x_continuous(breaks = seq(0,150,10)) +
  scale_y_continuous(breaks = seq(0,150,10)) +
  labs(x="Sepal Length", 
       y="Petal Length",
       title="Iris",
       subtitle="2 clusters")

g %>% ggplotly(tooltip = "text")
