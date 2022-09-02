library(tidyverse)
library(inspectdf)
library(mice)
library(caTools)
library(xgboost)
library(parsnip)
library(rBayesianOptimization)
library(Metrics)
set.seed(123)


df <- mlr3data::kc_housing

df %>% glimpse() 

df <- df %>%
  mutate(floors = as_factor(floors),
         waterfront = as_factor(waterfront),
         view = as_factor(view),
         condition = as_factor(condition),
         grade = as_factor(grade))

target <- "price"

df %>% inspect_na()


# Datanın bölünməsi - Splitting the data
split <- df %>% pull(target) %>% sample.split(SplitRatio = 0.8)
train <- df %>% subset(split == T)
test <- df %>% subset(split == F)


# "XGBoost" alqoritmasının qurulması - Fitting "XGBoost" algorithm ----  
regression <- boost_tree(
  mode = "regression") %>% 
  set_engine(engine = "xgboost") %>%
  fit(price ~ ., data = train)


# Proqnozlar
pred <- regression %>% 
  predict(test %>% select(-all_of(target))) %>% 
  pull(.pred)


# Reqressiya Modelinin Perormansının Qiymətləndirilməsi
actual <- test %>% pull(target)

eval_func <- function(x, y) summary(lm(y~x))
eval_sum <- eval_func(actual,pred)

eval_sum$adj.r.squared %>% round(2)
mae(actual,pred) %>% round(1)
rmse(actual,pred) %>% round(1)


# "Bayesian optimization" ilə optimal hiperparametrlərin tapılması ----
xgboost_fit <- function(mtry,
                        trees,
                        learn_rate,  
                        tree_depth) {
  regression = boost_tree(
    mode = "regression", 
    mtry = mtry,
    trees = trees,
    learn_rate = learn_rate,  
    tree_depth = tree_depth) %>% 
    set_engine(engine = "xgboost") %>%
    fit(price ~ ., data = train)
  
  pred = regression %>% 
    predict(test %>% select(-all_of(target))) %>% 
    pull(.pred)
  
  actual = test %>% pull(target)
  
  eval_sum = eval_func(actual,pred)
  
  score <- list(Score = eval_sum$adj.r.squared,
                Pred = 0)
}

search_bound_xgboost <- list(mtry = c(10L,100L),
                             trees = c(10L,150L),
                             learn_rate = c(0.01,0.5),  
                             tree_depth = c(2L,10L))

search_grid_xgboost <- data.frame(
  mtry = runif(30,10L,100L),
  trees = runif(30,10L,150L),
  learn_rate = runif(30,0.01,0.5),  
  tree_depth = runif(30,2L,10L) %>% round())

bayes_xgboost <- BayesianOptimization(
  FUN = xgboost_fit, 
  bounds = search_bound_xgboost,
  init_grid_dt = search_grid_xgboost,
  init_points = 5, 
  n_iter = 5)

obj <- bayes_xgboost$Best_Par


# Hiperparametr sazlama ilə modelləşdirmə - Modeling with hyperparametr tuning ----
regression <- boost_tree(
  mode = "regression", 
  mtry = obj[1],
  trees = obj[2],
  learn_rate = obj[3],  
  tree_depth = obj[4]) %>% 
  set_engine(engine = "xgboost") %>%
  fit(price ~ ., data = train)

# Proqnozlar
pred <- regression %>% 
  predict(test %>% select(-all_of(target))) %>% 
  pull(.pred)

# Reqressiya Modelinin Perormansının Qiymətləndirilməsi
actual <- test %>% pull(target)

eval_sum <- eval_func(actual,pred)

eval_sum$adj.r.squared %>% round(2)
mae(actual,pred) %>% round(1)
rmse(actual,pred) %>% round(1)
