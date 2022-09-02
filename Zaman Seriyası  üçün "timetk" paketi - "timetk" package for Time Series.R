library(tidyverse)
library(inspectdf)
library(timetk)
library(lubridate)

data <- walmart_sales_weekly %>%
  filter(id == "1_1") %>% 
  select(Date, Weekly_Sales) 

data %>% glimpse()

data %>% inspect_na()


# Time Series'i vizuallaşdırmaq ----
data %>% 
  plot_time_series(
    Date, Weekly_Sales, 
    # .color_var = lubridate::year(Date),
    # .color_lab = "Year",
    .interactive = T,
    .plotly_slider = T)

data %>% 
  mutate(Date = floor_date(Date, unit = "month")) %>% # day, week, month, quarter, year
  group_by(Date) %>% 
  summarise(Weekly_Sales = mean(Weekly_Sales)) %>% 
  ungroup() %>% 
  plot_time_series(
    .date_var = Date,
    .value = Weekly_Sales,
    .interactive = T)

# Mövsümiliyi yoxlamaq - To Check Seasonality ----
data %>%
  plot_seasonal_diagnostics(
    Date, Weekly_Sales, 
    #.feature_set = c("week", "month.lbl"),
    .interactive = T)

# Tarix dəyişənindən çoxlu dəyişənlər yaratmaq ----
data_tk <- data %>% tk_augment_timeseries_signature()

data_tk %>% glimpse()

data_tk %>% inspect_na()

df <- data_tk %>%
  select(-contains("hour"),
         -contains("day"),
         -minute,-second,-am.pm) %>% 
  mutate_if(is.ordered, as.character) %>% 
  mutate_if(is.character,as_factor)

# Hədəf dəyişəndə outlier'lərə baxmaq ----
df %>%
  plot_anomaly_diagnostics(
    .date = Date,
    .value = Weekly_Sales,
    .interactive = T,
    .title = "Anomaly Diagnostics",
    .anom_color ="red", 
    .max_anomalies = 0.1)


# Period ilə datanın bölünməsi - Splitting the data by period ----
splits <- df %>% 
  time_series_split(assess = "6 month", cumulative = T)

train <- splits %>% training()
test  <- splits %>% testing()
