# Multinomial Klassifikasiya ----

library(tidyverse)
library(h2o)
library(highcharter)

# H20
h2o.init()

h2o_data <- iris %>% as.h2o()


# Datanın bölünməsi - Splitting the data
h2o_data <- h2o_data %>% h2o.splitFrame(ratios = 0.8, seed = 123)
train <- h2o_data[[1]]
test <- h2o_data[[2]]

target <- "Species"
features <- iris %>% select(-Species) %>% names()


# AutoML
model <- h2o.automl(
  x = features, y = target,
  training_frame = train,
  validation_frame = test,
  leaderboard_frame = test,
  stopping_metric = "mean_per_class_error",
  nfolds = 10, seed = 123,
  balance_classes = TRUE,
  max_runtime_secs = 180)

model@leaderboard %>% as.data.frame()
leader_model <- model@leader 


# Proqnozlar
pred <- leader_model %>% h2o.predict(test) %>% as.data.frame()


# Qarışıqlıq matrisi - Confision Matrice
actuals <- test %>% as.data.frame() %>% pull(Species)
predictions <- pred$predict

actuals %>% table()
predictions %>% table()

cm <- table(actuals,predictions)


# "Overfitting" / "Underfitting" 
leader_model %>%
  h2o.mean_per_class_error(
    train = T, 
    valid = T, 
    xval = T) %>%
  round(3) 


# Önəmli Dəyişən - Variable Importance
leader_model %>% 
  h2o.varimp() %>% 
  as.data.frame() %>% 
  .[.$percentage != 0,] %>%
  select(variable, percentage) %>%
  hchart("pie", hcaes(x = variable, y = percentage)) %>%
  hc_colors(colors = "orange") %>%
  hc_xAxis(visible=T) %>%
  hc_yAxis(visible=T)
