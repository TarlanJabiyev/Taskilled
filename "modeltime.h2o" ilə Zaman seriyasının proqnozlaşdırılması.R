library(tidyverse)
library(inspectdf)
library(timetk)
library(lubridate)
library(tidymodels)
library(modeltime.h2o)
library(rstudioapi)
library(modeltime.ensemble)

data <- walmart_sales_weekly %>%
  select(id, Date, Weekly_Sales)


data %>% glimpse()

data %>% inspect_na()


# Time Series'i vizuallaşdırmaq
data %>% 
  group_by(id) %>% 
  plot_time_series(
    Date, Weekly_Sales,
    .facet_ncol  = 2,
    .smooth      = F,
    .interactive = T)


convert_date_ts <- function(data, unit = "day"){
  new_data <- data %>% 
    mutate(Date = floor_date(Date, unit = unit)) %>% 
    group_by(Date, id) %>% 
    summarise(Weekly_Sales = mean(Weekly_Sales)) %>% 
    ungroup()
  return(new_data)}

# Həftəlik
data %>% 
  convert_date_ts(unit = "week") %>% 
  group_by(id) %>% 
  plot_time_series(
    .date_var = Date,
    .value = Weekly_Sales,
    .facet_ncol = 2,
    .smooth_size = 0.5)

# Aylıq
data %>% 
  convert_date_ts(unit = "month") %>% 
  group_by(id) %>% 
  plot_time_series(
    .date_var = Date,
    .value = Weekly_Sales,
    .facet_ncol = 2,
    .smooth_size = 0.5)

# Rüblük
data %>% 
  convert_date_ts(unit = "quarter") %>% 
  group_by(id) %>% 
  plot_time_series(
    .date_var = Date,
    .value = Weekly_Sales,
    .facet_ncol = 2,
    .smooth_size = 0.5)

# İllik
data %>% 
  convert_date_ts(unit = "year") %>% 
  group_by(id) %>% 
  plot_time_series(
    .date_var = Date,
    .value = Weekly_Sales,
    .facet_ncol = 2,
    .smooth_size = 0.5)


# Hədəf dəyişəndə outlier'lərə baxmaq
data %>%
  group_by(id) %>%
  plot_anomaly_diagnostics(
    .date = Date,
    .value = Weekly_Sales,
    .facet_ncol = 2,
    .interactive = T,
    .title = "Anomaly Diagnostics Dow Jones",
    .anom_color ="#FB3029", 
    .max_anomalies = 0.1, 
    .alpha = 0.05)


# Mövsümiliyi yoxlamaq - To Check Seasonality
data %>%
  filter(id == "1_8") %>% 
  plot_seasonal_diagnostics(
    Date, Weekly_Sales,
    .feature_set = c("week", "month.lbl"),
    .interactive = T)


# Period ilə datanın bölünməsi - Splitting the data by period
# Tarix dəyişənindən çoxlu dəyişənlər yaratmaq
splits <- data %>% 
  time_series_split(assess = "3 month", cumulative = T)

recipe_spec <- recipe(Weekly_Sales ~ ., training(splits)) %>%
  step_timeseries_signature(Date) 

train <- training(splits) %>% bake(prep(recipe_spec),.)
test  <- testing(splits) %>% bake(prep(recipe_spec),.)


# "modeltime.h2o" ilə eyni zamanda birdən çox ID üçün Zaman Seriyası modellərinin qurulması ----
h2o.init()

model_spec_h2o <- automl_reg(mode = 'regression') %>%
  set_engine(
    'h2o', max_runtime_secs = 360,
    nfolds = 5, seed = 123, 
    verbosity = NULL, max_models = 3, 
    exclude_algos = "GLM",
    max_runtime_secs_per_model = 3) 

model_fit_h2o <- model_spec_h2o %>%
  fit(Weekly_Sales ~ ., train)

# Proqnoz
Prediction <- model_fit_h2o %>% predict(test)

# İrəli proqnoz
modeltime <- model_fit_h2o %>% modeltime_table() 

modeltime %>%
  modeltime_calibrate(test) %>%
  modeltime_forecast(
    new_data = test,
    actual_data = data,
    keep_data = T
  ) %>%
  group_by(id) %>%
  plot_modeltime_forecast(
    .facet_ncol = 2, 
    .interactive = T)


# Algoritmaya bütün datanı öyrətmək - Refit to full dataset ----
data_prepared <- bind_rows(train, test)

future <- data_prepared %>%
  group_by(id) %>%
  future_frame(.length_out = "6 month") %>%
  ungroup()

future_prepared <- recipe_spec %>% prep() %>% bake(future)

refit <- modeltime %>% modeltime_refit(data_prepared)

# Plot
refit %>%
  modeltime_forecast(
    new_data = future_prepared,
    actual_data = data_prepared,
    keep_data = T
  ) %>%
  group_by(id) %>%
  plot_modeltime_forecast(
    .facet_ncol = 2,
    .interactive = T)

# Modeli saxla və yüklə - Save and Load Model ----
path <- dirname(getSourceEditorContext()$path)

model_fit_h2o %>% 
  save_h2o_model(path = paste0(path,"/model_ts_h2o"), overwrite = T)

model <- load_h2o_model(path = paste0(path,"/model_ts_h2o"))

