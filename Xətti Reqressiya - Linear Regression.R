library(tidyverse) 
library(data.table)
library(inspectdf)
library(mice)
library(recipes) 
library(graphics)
library(caret) 
library(h2o)  
library(Metrics)
library(plotly)
library(glue)
library(patchwork)

data <- mlr3data::moneyball


# Datanın təmizlənməsi - Data Cleaning ----

data %>% glimpse()

data <- data %>% 
  mutate(year = as_factor(year),
         rankseason = as_factor(rankseason),
         rankplayoffs = as_factor(rankplayoffs))


data %>% inspect_na()

data %>% 
  inspect_na() %>% 
  filter(pcnt < 70) %>% 
  pull(col_name) -> variables

data <- data %>% select(all_of(variables))


target <- "rs"


df.num <- data %>%
  select_if(is.numeric) %>%
  select(all_of(target),everything())

df.chr <- data %>%
  mutate_if(is.factor,as.character) %>% 
  select_if(is.character)


df.num %>% inspect_na()

df.num_mice <- df.num %>% mice(method = "rf", seed = 123)
df.num <- df.num_mice %>% complete()


df.chr %>% inspect_na()

# rec_obj <- recipe(~ ., data = df.chr) %>%
#   step_impute_mode(all_nominal()) %>%
#   prep(stringsAsFactors = FALSE)
# 
# df.chr <- bake(rec_obj, df.chr)


# Kəmiyyət dəyişənlərdə Outlier'ların həll olunması ----
solve_outliers <- function(data, target) {
  num_vars <- data %>% 
    select(-all_of(target)) %>% 
    names()
  
  for_vars <- c()
  for (b in 1:length(num_vars)) {
    OutVals <- boxplot(data[[num_vars[b]]], plot=F)$out
    if(length(OutVals)>0){
      for_vars[b] <- num_vars[b]
    }
  }
  for_vars <- for_vars %>% as.data.frame() %>% drop_na() %>% pull(.)
  for_vars %>% length()
  
  for (o in for_vars) {
    OutVals <- boxplot(data[[o]], plot=F)$out
    mean <- mean(data[[o]],na.rm=T)
    
    o3 <- ifelse(OutVals>mean,OutVals,NA) %>% na.omit() %>% as.matrix() %>% .[,1]
    o1 <- ifelse(OutVals<mean,OutVals,NA) %>% na.omit() %>% as.matrix() %>% .[,1]
    
    val3 <- quantile(data[[o]], 0.75, na.rm = T) + 1.5 * IQR(data[[o]], na.rm = T)
    data[which(data[[o]] %in% o3),o] <- val3
    
    val1 <- quantile(data[[o]], 0.25, na.rm = T) - 1.5*IQR(data[[o]], na.rm = T)
    data[which(data[[o]] %in% o1),o] <- val1
  }
  return(data)
}

df.num <- df.num %>% solve_outliers(target = target)


# Keyfiyyət dəyişənlər üçün "One Hote Encoding" ----
df.chr <- dummyVars(" ~ .", df.chr) %>% 
  predict(df.chr) %>% 
  as.data.frame()

df <- cbind(df.chr,df.num) %>%
  select(all_of(target),everything())


# "Multicollinearity" probleminin həll olunması ----

features <- df %>% select(-all_of(target)) %>% names()

f <- as.formula(paste(target, paste(features, collapse = " + "), sep = " ~ "))
glm <- glm(f, data = df)

glm %>% summary()

coef_na <- attributes(alias(glm)$Complete)$dimnames[[1]]
features <- features[!features %in% coef_na]

f <- as.formula(paste(target, paste(features, collapse = " + "), sep = " ~ "))
glm <- glm(f, data = df)

glm %>% summary()

# VIF (Variance Inflation Factor) 
while(glm %>% faraway::vif() %>% sort(decreasing = T) %>% .[1] >= 2){
  afterVIF <- glm %>% faraway::vif() %>% sort(decreasing = T) %>% .[-1] %>% names()
  f <- as.formula(paste(target, paste(afterVIF, collapse = " + "), sep = " ~ "))
  glm <- glm(f, data = df)
}

glm %>% faraway::vif() %>% sort(decreasing = T) %>% names() -> features 

df <- df %>% select(all_of(target),all_of(features))


# Standartlaşdırma / Normallaşdırma ----
df %>% glimpse()

df[,-1] <- df[,-1] %>% scale() %>% as.data.frame()


# Datanın bölünməsi - Splitting the data ----
h2o.init()

h2o_data <- df %>% as.h2o()

h2o_data <- h2o_data %>% h2o.splitFrame(ratios = 0.8, seed = 123)
train <- h2o_data[[1]]
test <- h2o_data[[2]]

features <- df %>% select(-all_of(target)) %>% names()


# GLM (Generalized Linear Model) alqoritmasının qurulması və Çarpaz Validasiya -----
# Fitting GLM (Generalized Linear Model) algorithm and Cross Validation
model <- h2o.glm(
  x = features, y = target,
  training_frame = train,
  validation_frame = test,
  nfolds = 10, seed = 123,
  lambda = 0, compute_p_values = T)


# Proqnozlaşdırıcıların əhəmiyyət səviyyələri - Significance levels of predictors ----
model@model$coefficients_table %>%
  as.data.frame() %>%
  select(names,p_value) %>%
  mutate(p_value = round(p_value,3)) %>%
  .[-1,] %>%
  arrange(desc(p_value))

# P dəyərinin əhəmiyyət səviyyələri - Significance levels of P_value:
# 0    <= p_val < 0.001 ***
# 0.001 < p_val < 0.05  **
# 0.05  < p_val < 0.01  *
# 0.01  < p_val < 0.1   .

# Addım-addım geriyə doğru aradan qaldırılma - Stepwise Backward Elimination
while(model@model$coefficients_table %>%
      as.data.frame() %>%
      select(names,p_value) %>%
      mutate(p_value = round(p_value,3)) %>%
      .[-1,] %>%
      arrange(desc(p_value)) %>%
      .[1,2] >= 0.05) {
  model@model$coefficients_table %>%
    as.data.frame() %>%
    select(names,p_value) %>%
    mutate(p_value = round(p_value,3)) %>%
    filter(!is.nan(p_value)) %>%
    .[-1,] %>%
    arrange(desc(p_value)) %>%
    .[1,1] -> v
  features <- features[features!=v]
  
  train_h2o <- train %>% as.data.frame() %>% select(all_of(target),all_of(features)) %>% as.h2o()
  test_h2o <- test %>% as.data.frame() %>% select(all_of(target),all_of(features)) %>% as.h2o()
  
  model <- h2o.glm(
    x = features, y = target,
    training_frame = train,
    validation_frame = test,
    nfolds = 10, seed = 123,
    lambda = 0, compute_p_values = T)
}
model@model$coefficients_table %>%
  as.data.frame() %>%
  select(names,p_value) %>%
  mutate(p_value = round(p_value,3)) 


# Proqnozlar ----
y_pred <- model %>% h2o.predict(test) %>% as.data.frame()
pred <- y_pred$predict


# Reqressiya Modelinin Perormansının Qiymətləndirilməsi ----
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
  theme(plot.title = element_text(color = "darkgreen", size=16, hjust=0.5),
        axis.text.y = element_text(size=12), 
        axis.text.x = element_text(size=12),
        axis.title.x = element_text(size=14), 
        axis.title.y = element_text(size=14))

g %>% ggplotly()


# "Overfitting" / "Underfitting" ----
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
