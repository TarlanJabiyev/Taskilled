# "AutoML" ilə Klassifikasiya ----

library(tidyverse)
library(rstudioapi)
library(car)
library(inspectdf)
library(h2o)
library(highcharter)
library(lime)


path <- dirname(getSourceEditorContext()$path)
setwd(path)

data <- read_csv("churn.csv")

data %>% glimpse()

data$y <- data$y %>%
  recode(" 'yes' = 1; 'no' = 0") %>%
  as_factor()

data$y %>% table() %>% prop.table() %>% round(2)


data %>% inspect_na()


# H20
h2o.init()

h2o_data <- data %>% as.h2o()


# Datanın bölünməsi - Splitting the data
h2o_data <- h2o_data %>% h2o.splitFrame(ratios = 0.8, seed = 123)
train <- h2o_data[[1]]
test <- h2o_data[[2]]

target <- "y"
features <- data %>% select(-y,-id) %>% names()


# AutoML
model <- h2o.automl(
  x = features, y = target,
  training_frame = train,
  validation_frame = test,
  leaderboard_frame = test,
  stopping_metric = "AUC",
  nfolds = 10, seed = 123,
  balance_classes = TRUE,
  exclude_algos = "GLM",
  max_runtime_secs = 300)

model@leaderboard %>% as.data.frame()
leader_model <- model@leader 

# Proqnozlar
pred <- leader_model %>% h2o.predict(test) %>% as.data.frame()

# "Overfitting" / "Underfitting" 
leader_model %>%
  h2o.auc(train = T,
          valid = T,
          xval = T) %>%
  as_tibble() %>%
  round(2) %>%
  mutate(data = c("train","test","cross_val")) %>%
  mutate(gini = 2 * value - 1) %>%
  select(data, auc = value, gini)


# Önəmli Dəyişən - Variable Importance ----
leader_model %>% 
  h2o.varimp() %>% 
  as.data.frame() %>% 
  .[.$percentage != 0,] %>%
  select(variable, percentage) %>%
  hchart("pie", hcaes(x = variable, y = percentage)) %>%
  hc_colors(colors = "orange") %>%
  hc_xAxis(visible=T) %>%
  hc_yAxis(visible=T)


# Qara qutunun izahı - Explanation of "Black Box"  ----
ids <- test %>% as.data.frame() %>% pull(id) %>% unique()

explanation <- function(train, features, leader_model, column, cases, n) {
  lime_obj <- train %>% as.data.frame() %>% 
    select(all_of(features)) %>% 
    lime(leader_model, bin_continuous = F)
  
  test <- test %>% as.data.frame()
  
  expleaner <- test[test[[column]] %in% cases,] %>% 
    select(all_of(features)) %>% 
    lime::explain(lime_obj, n_labels = 1, n_features = n)
  
  return(expleaner %>% plot_features())
}

explanation(train = train, 
            features = features, 
            leader_model = leader_model,
            column = "id",
            cases = ids[1:6],
            n = 4)


# Modeli saxla və yüklə - Save and Load Model ----
path

leader_model %>% h2o.saveModel(path = path)

saved_model <- h2o.loadModel("")
saved_model %>% h2o.auc()
