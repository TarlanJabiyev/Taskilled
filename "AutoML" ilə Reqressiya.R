# "AutoML" ilə Reqressiya ----

library(tidyverse) 
library(data.table)
library(inspectdf)
library(h2o) 
library(Metrics)
library(glue)
library(plotly)
library(patchwork)

data <- mlr3data::moneyball

data %>% glimpse()

data <- data %>% 
  mutate(year = as_factor(year),
         rankseason = as_factor(rankseason),
         rankplayoffs = as_factor(rankplayoffs))


data %>% inspect_na()


# H2O
h2o.init()

h2o_data <- data %>% as.h2o()


# Datanın bölünməsi - Splitting the data
h2o_data <- h2o_data %>% h2o.splitFrame(ratios = 0.8, seed = 123)
train <- h2o_data[[1]]
test <- h2o_data[[2]]

target <- "rs"
features <- data %>% select(-all_of(target)) %>% names()

# AutoML
model <- h2o.automl(
  x = features,
  y = target,
  training_frame    = train,
  validation_frame  = test,
  leaderboard_frame = test,
  stopping_metric = "MAE",
  seed = 123,
  max_runtime_secs = 360)

model@leaderboard %>% as.data.frame()
model <- model@leader


# Proqnozlar
y_pred <- model %>% h2o.predict(test) %>% as.data.frame()
pred <- y_pred$predict


# Reqressiya Modelinin Perormansının Qiymətləndirilməsi
actual <- test %>% as.data.frame() %>% pull(all_of(target))

eval_func <- function(x, y) summary(lm(y~x))
eval_sum <- eval_func(actual,pred)

eval_sum$adj.r.squared %>% round(2)
mae(actual,pred) %>% round(1)
rmse(actual,pred) %>% round(1)


# Plot
results <- cbind(pred,actual) %>% 
  as.data.frame()

Adjusted_R2 <- eval_sum$adj.r.squared

g <- results %>% 
  ggplot(aes(pred, actual)) + 
  geom_point(color = "darkred") + 
  geom_smooth(method = lm) + 
  labs(x = "Proqnoz dəyərlər", 
       y = "Əsl dəyərlər",
       title = glue("Test: Adjusted R2 = {round(enexpr(Adjusted_R2),2)}")) +
  theme(plot.title = element_text(color="darkgreen", size=16, hjust=0.5),
        axis.text.y = element_text(size=12), 
        axis.text.x = element_text(size=12),
        axis.title.x = element_text(size=14), 
        axis.title.y = element_text(size=14))

g %>% ggplotly()


# "Overfitting" / "Underfitting"
y_pred_train <- model %>% h2o.predict(train) %>% as.data.frame()
pred_train <- y_pred_train$predict

actual_train <- train %>% as.data.frame() %>% pull(all_of(target))

eval_sum <- eval_func(actual_train,pred_train)

eval_sum$adj.r.squared %>% round(2)
mae(actual_train,pred_train) %>% round(1)
rmse(actual_train,pred_train) %>% round(1)


# Plot
results_train <- cbind(pred_train,actual_train) %>% 
  as.data.frame()

Adjusted_R2_train <- eval_sum$adj.r.squared

g_train <- results_train %>% 
  ggplot(aes(pred_train, actual_train)) + 
  geom_point(color = "darkred") + 
  geom_smooth(method = lm) + 
  labs(x = "Proqnoz dəyərlər", 
       y = "Əsl dəyərlər",
       title = glue("Train: Adjusted R2 = {round(enexpr(Adjusted_R2_train),2)}")) +
  theme(plot.title = element_text(color="darkgreen", size=16, hjust=0.5),
        axis.text.y = element_text(size=12), 
        axis.text.x = element_text(size=12),
        axis.title.x = element_text(size=14), 
        axis.title.y = element_text(size=14))

g_train %>% ggplotly()


# Müqayisə 
g_train + g

tibble(Adjusted_R2_train,
       Adjusted_R2_test = Adjusted_R2)
