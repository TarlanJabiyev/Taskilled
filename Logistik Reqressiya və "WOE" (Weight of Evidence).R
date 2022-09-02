library(tidyverse)
library(rstudioapi)
library(car)
library(inspectdf)
library(caret)
library(glue)
library(highcharter)
library(h2o)
library(scorecard)

path <- dirname(getSourceEditorContext()$path)
setwd(path)

data <- read_csv("churn.csv")


# Datanın Önhazırlığı - Data Preprocessing ----

data %>% glimpse()


data$y <- data$y %>%
  recode(" 'yes' = 1; 'no' = 0") %>%
  as_factor()

data$y %>% table() %>% prop.table() %>% round(2)


data %>% inspect_na()


target <- "y"
exclude <- c("id")


# "Weight Of Evidence" ----

# IV (information values) 
iv <- data %>% 
  select(-all_of(exclude)) %>% 
  iv(y = target) %>% 
  as_tibble() %>%
  mutate(info_value = round(info_value, 3)) %>%
  arrange(desc(info_value))

# IV dəyəri 0.02'dən kiçik olan dəyişənləri çıxarmaq
ivars <- iv %>% 
  filter(info_value > 0.02) %>% 
  pull(variable) 

df.iv <- data %>% select(all_of(target),all_of(ivars))

df.iv %>% dim()

# Datanın bölünməsi
dt_list <- df.iv %>% 
  split_df(target, ratio = 0.8, seed = 123)

# woe binning 
bins <- dt_list$train %>% woebin(target)

bins$age %>% as_tibble()
bins$age %>% woebin_plot()
bins$marital %>% as_tibble()
bins$marital %>% woebin_plot()
bins$job %>% as_tibble()
bins$job %>% woebin_plot()

train_woe <- dt_list$train %>% woebin_ply(bins) 
test_woe <- dt_list$test %>% woebin_ply(bins)

names <- train_woe %>% 
  names() %>% 
  str_replace_all("_woe","")  

names(train_woe) <- names
names(test_woe) <- names


# "Multicollinearity" probleminin həll olunması ----

solve_multicollinearity <- function(data,target) {
  features <- data %>% select(-all_of(target)) %>% names()
  
  f <- as.formula(paste(target, paste(features, collapse = " + "), sep = " ~ "))
  glm <- glm(f, data = data, family = "binomial")
  
  coef_na <- attributes(alias(glm)$Complete)$dimnames[[1]]
  features <- features[!features %in% coef_na]
  f <- as.formula(paste(target, paste(features, collapse = " + "), sep = " ~ "))
  glm <- glm(f, data = data, family = "binomial")
  
  while(glm %>% vif() %>% arrange(desc(gvif)) %>% .[1,2] >= 2){
    afterVIF <- glm %>% vif() %>% arrange(desc(gvif)) %>% pull(variable) %>% .[-1]
    f <- as.formula(paste(target, paste(afterVIF, collapse = " + "), sep = " ~ "))
    glm <- glm(f, data = data, family = "binomial")
  }
  
  return(glm %>% vif() %>% pull(variable))
}

features <- train_woe %>% solve_multicollinearity(target)


# GLM (Generalized Linear Model) alqoritmasının qurulması - Fitting GLM (Generalized Linear Model) algorithm ----
h2o.init()

train_h2o <- train_woe %>% select(target,all_of(features)) %>% as.h2o()
test_h2o <- test_woe %>% select(target,all_of(features)) %>% as.h2o()

model <- h2o.glm(
  x = features, y = target, family = "binomial", 
  training_frame = train_h2o, validation_frame = test_h2o,
  nfolds = 10, seed = 123, remove_collinear_columns = T,
  balance_classes = T, lambda = 0, compute_p_values = T)


# Proqnozlaşdırıcıların əhəmiyyət səviyyələri - Significance levels of predictors

while(model@model$coefficients_table %>%
      as.data.frame() %>%
      select(names,p_value) %>%
      mutate(p_value = round(p_value,3)) %>%
      .[-1,] %>%
      arrange(desc(p_value)) %>%
      .[1,2] >= 0.05){
  model@model$coefficients_table %>%
    as.data.frame() %>%
    select(names,p_value) %>%
    mutate(p_value = round(p_value,3)) %>%
    filter(!is.nan(p_value)) %>%
    .[-1,] %>%
    arrange(desc(p_value)) %>%
    .[1,1] -> v
  features <- features[features!=v]
  
  train_h2o <- train_woe %>% select(target,all_of(features)) %>% as.h2o()
  test_h2o <- test_woe %>% select(target,all_of(features)) %>% as.h2o()
  
  model <- h2o.glm(
    x = features, y = target, family = "binomial", 
    training_frame = train_h2o, validation_frame = test_h2o,
    nfolds = 10, seed = 123, remove_collinear_columns = T,
    balance_classes = T, lambda = 0, compute_p_values = T)
}
model@model$coefficients_table %>%
  as.data.frame() %>%
  select(names,p_value) %>%
  mutate(p_value = round(p_value,3))

model@model$coefficients %>%
  as.data.frame() %>%
  mutate(names = rownames(model@model$coefficients %>% as.data.frame())) %>%
  `colnames<-`(c("coefficients","names")) %>%
  select(names,coefficients) %>% 
  as_tibble()

h2o.varimp(model) %>% as.data.frame() %>% 
  filter(percentage != 0) %>% 
  select(variable, percentage) %>%
  hchart("pie", hcaes(x = variable, y = percentage)) %>%
  hc_colors(colors = "orange") %>%
  hc_xAxis(visible=T) %>%
  hc_yAxis(visible=T)


# Proqnozlar ----

pred <- model %>% h2o.predict(test_h2o) %>% 
  as.data.frame() %>% select(p1,predict)


# Optimal "Threshold" ----
pred[pred$predict == 0, "p1"] %>% max()
pred[pred$predict == 1, "p1"] %>% min()

model %>% h2o.performance(test_h2o) %>%
  h2o.find_threshold_by_max_metric("f1")


# Klassifikasiya Modelinin Perormansının Qiymətləndirilməsi ----

# Qarışıqlıq matrisi - Confision Matrice
actuals <- dt_list$test %>% pull(target)
predictions <- pred$predict

actuals %>% table()
predictions %>% table()

cm <- table(actuals,predictions)

tp <- cm[2,2]
tn <- cm[1,1]
fp <- cm[1,2]
fn <- cm[2,1]

precision <- tp / (tp + fp)
recall_sensitivity <- tp / (tp + fn)
specifity <- tn / (tn + fn)
accuracy <- (tp + tn) / (tp + tn + fp + fn)
f1_score <- 2 * precision * recall_sensitivity / (precision + recall_sensitivity)
balanced_accuracy <- (recall_sensitivity + specifity) / 2

tibble(precision,recall_sensitivity,specifity,
       accuracy,f1_score,balanced_accuracy)

# Area Under Curve (AUC) və ROC əyrisi

model %>% 
  h2o.performance(test_h2o) %>% 
  h2o.metric() %>% 
  select(threshold, precision, recall, tpr, fpr) %>% 
  add_column(random_tpr = runif(nrow(.), min=0.001, max=1)) %>% 
  mutate(random_fpr = random_tpr) %>% 
  arrange(random_tpr, random_fpr) -> metrics

model %>% 
  h2o.performance(test_h2o) %>% 
  h2o.auc() %>% round(2) -> auc

highchart() %>% 
  hc_add_series(metrics, "scatter", hcaes(y=tpr,x=fpr), color='green', name='TPR') %>%
  hc_add_series(metrics, "line", hcaes(y=random_tpr,x=random_fpr), color='red', name='Təsadüfi təxmin') %>% 
  hc_add_annotation(
    labels = list(
      point = list(xAxis=0,yAxis=0,x=0.3,y=0.6),
      text = glue('AUC = {enexpr(auc)}'))
  ) %>%
  hc_title(text = "ROC əyrisi") %>% 
  hc_subtitle(text = "Model təsadüfi təxmindən daha yaxşı performans göstərir") 


# "Overfitting" / "Underfitting" ----
model %>%
  h2o.auc(train = T,
          valid = T,
          xval = T) %>%
  as_tibble() %>%
  round(2) %>%
  mutate(data = c("train","test","cross_val")) %>%
  mutate(gini = 2 * value - 1) %>%
  select(data, auc = value, gini)
