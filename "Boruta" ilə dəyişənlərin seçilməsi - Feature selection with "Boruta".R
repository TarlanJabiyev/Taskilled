library(tidyverse)
library(Boruta)

df <- read_csv("churn.csv")

df %>% glimpse()

df <- df %>% 
  select(-id) %>% 
  mutate(y = as_factor(y))


# Boruta alqoritması ilə dəyişənlərin seçilməsi
set.seed(123)
boruta <- Boruta(y ~ ., data = df) %>% 
  TentativeRoughFix()
boruta


# Seçilən dəyişənlər
boruta.df <- boruta %>% attStats()
boruta.df %>% 
  filter(decision == "Confirmed") %>% 
  rownames()


# Plot
boruta %>% plot(xlab = "", xaxt = "n")
list <- list()
for (i in colnames(boruta$ImpHistory)) { 
  list[[i]] <- boruta$ImpHistory[is.finite(boruta$ImpHistory[,i]),i]
}
axis(side = 1, las = 2, labels = names(list),
     at = 1:ncol(boruta$ImpHistory), cex.axis = 0.7)
