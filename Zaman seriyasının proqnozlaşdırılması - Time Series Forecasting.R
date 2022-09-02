library(tidyverse)
library(inspectdf)
library(timetk)
library(lubridate)
library(highcharter)
library(h2o)
library(tidymodels)
library(modeltime)

data <- walmart_sales_weekly %>%
  filter(id == "1_1") %>% 
  select(Date, Weekly_Sales) 

data %>% glimpse()

data %>% inspect_na()


# Tarix dəyişənindən çoxlu dəyişənlər yaratmaq
data_tk <- data %>% tk_augment_timeseries_signature()

data_tk %>% glimpse()

data_tk %>% inspect_na()

df <- data_tk %>%
  select(-contains("hour"),
         -contains("day"),
         -minute,-second,-am.pm) %>% 
  mutate_if(is.ordered, as.character) %>% 
  mutate_if(is.character,as_factor)

# Period ilə datanın bölünməsi - Splitting the data by period
splits <- df %>% 
  time_series_split(assess = "6 month", cumulative = T)

train <- splits %>% training()
test  <- splits %>% testing()


# "h2o" ilə Time Series ----
h2o.init()    

train_h2o <- train %>% as.h2o()
test_h2o <- test %>% as.h2o()

y <- "Weekly_Sales" 
x <- df %>% select(-Weekly_Sales) %>% names()

model_h2o <- h2o.automl(
  x = x, y = y, 
  training_frame = train_h2o, 
  validation_frame = test_h2o,
  leaderboard_frame = test_h2o,
  stopping_metric = "RMSE",
  seed = 123, nfolds = 10,
  exclude_algos = "GLM",
  max_runtime_secs = 360) 

model_h2o@leaderboard %>% as.data.frame() 
h2o_leader <- model_h2o@leader

pred_h2o <- h2o_leader %>% h2o.predict(test_h2o) 

h2o_leader %>% 
  h2o.rmse(train = T,
           valid = T,
           xval = T)

error_tbl <- df %>% 
  filter(Date >= min(test$Date)) %>% 
  add_column(pred = pred_h2o %>% as_tibble() %>% pull(predict)) %>%
  select(Date, actual = Weekly_Sales, pred)

highchart() %>% 
  hc_xAxis(categories = error_tbl$Date) %>% 
  hc_add_series(data=error_tbl$actual, type='line', color='red', name='Actual') %>% 
  hc_add_series(data=error_tbl$pred, type='line', color='green', name='Predicted') %>% 
  hc_title(text='Predict')


# Proqnoz
new_data <- seq(as.Date("2012/11/01"), as.Date("2013/05/01"), "week") %>%
  as_tibble() %>% 
  add_column(Weekly_Sales=0) %>% 
  rename(Date=value) %>% 
  tk_augment_timeseries_signature() %>% 
  select(-contains("hour"),
         -contains("day"),
         -minute,-second,-am.pm) %>% 
  mutate_if(is.ordered, as.character) %>% 
  mutate_if(is.character,as_factor)

new_h2o <- new_data %>% as.h2o()

new_predictions <- h2o_leader %>% 
  h2o.predict(new_h2o) %>% 
  as_tibble() %>%
  add_column(Date=new_data$Date) %>% 
  select(Date, Weekly_Sales = predict) 

df %>% 
  bind_rows(new_predictions) %>% 
  mutate(colors=c(rep('Actual',nrow(df)),rep('Predicted',nrow(new_predictions)))) %>% 
  hchart("line", hcaes(Date, Weekly_Sales, group = colors)) %>% 
  hc_title(text='Forecast') %>% 
  hc_colors(colors = c('red','green'))


# "modeltime" ilə Time Series ----

train <- data %>% filter(Date < "2012-05-01")
test <- data %>% filter(Date >= "2012-05-01")

# 1.Auto ARIMA
model_fit_arima <- arima_reg() %>%
  set_engine("auto_arima") %>%
  fit(Weekly_Sales ~ Date, train)


# 2. Prophet
model_fit_prophet <- prophet_reg(seasonality_yearly = TRUE) %>%
  set_engine("prophet") %>%
  fit(Weekly_Sales ~ Date, train)


# Modelləşdirmə üçün datanın hazırlanması - Preprocessing data for modeling
recipe_spec <- recipe(Weekly_Sales ~ Date, train) %>%
  step_timeseries_signature(Date) %>%
  step_fourier(Date, period = 180, K = 2) %>%
  step_dummy(all_nominal())

recipe_spec %>% prep() %>% juice() %>% View()


# 3.GLM Net
model_spec_glmnet <- linear_reg(penalty = 0.01, mixture = 0.5) %>%
  set_engine("glmnet")

workflow_fit_glmnet <- workflow() %>%
  add_model(model_spec_glmnet) %>%
  add_recipe(recipe_spec %>% step_rm(Date)) %>%
  fit(train)


# 4.Random Forest
model_spec_rf <- rand_forest(trees = 500, min_n = 50) %>%
  set_engine("randomForest")

workflow_fit_rf <- workflow() %>%
  add_model(model_spec_rf) %>%
  add_recipe(recipe_spec %>% step_rm(Date)) %>%
  fit(train)


# 5.Prophet Boost
model_spec_prophet_boost <- prophet_boost(seasonality_yearly = T) %>%
  set_engine("prophet_xgboost") 

workflow_fit_prophet_boost <- workflow() %>%
  add_model(model_spec_prophet_boost) %>%
  add_recipe(recipe_spec) %>%
  fit(train)


# Proqnoz
models <- modeltime_table(
  model_fit_arima,
  model_fit_prophet,
  workflow_fit_glmnet,
  workflow_fit_rf,
  workflow_fit_prophet_boost) %>%
  modeltime_calibrate(test)

models %>% 
  #filter(.model_id == 2) %>% 
  modeltime_forecast(actual_data = df) %>%
  plot_modeltime_forecast(.interactive = T,
                          .plotly_slider = T)


# Performans qiymətləndirməsi
models %>% modeltime_accuracy() %>% 
  table_modeltime_accuracy(.interactive = F)


# İrəli proqnoz
models %>%
  filter(.model_id %in% 2) %>%
  modeltime_refit(df) %>%
  modeltime_forecast(h = "6 month", 
                     actual_data = df) %>%
  select(-contains("conf")) %>% 
  plot_modeltime_forecast(.interactive = T,
                          .plotly_slider = T,
                          .legend_show = F)
