library(tidyverse)
library(h2o)
h2o.init()

train <- h2o.importFile(
  path = "http://h2o-public-test-data.s3.amazonaws.com/smalldata/anomaly/ecg_discord_train.csv", 
  header = F, sep = ",")
test <- h2o.importFile(
  path = "http://h2o-public-test-data.s3.amazonaws.com/smalldata/anomaly/ecg_discord_test.csv", 
  header = F, sep = ",")

train %>% as_tibble() %>% dim()
train %>% as_tibble() %>% glimpse()

test %>% as_tibble() %>% dim()
test %>% as_tibble() %>% glimpse()


# "h2o.deeplearning" ilə "autoencoder" modelləşdirmə ----
model <- h2o.deeplearning(
  x = names(train),
  training_frame = train,
  autoencoder = T,
  activation = "Tanh",
  hidden = c(50,20,50),
  sparse = T,
  l1 = 0.001,
  epochs = 10)

model %>% h2o.varimp_plot()

# model %>% 
#   h2o.varimp() %>% 
#   arrange(desc(percentage))


# h2o.deeplearning ilə anomaliyaların aşkarlanması və vizuallaşdırılması ----
anomalialar <- model %>% h2o.anomaly(test) %>% as.data.frame()

anomalialar %>% plot.ts()

anomalialar %>% 
  rownames_to_column() %>%
  mutate(outlier = ifelse(Reconstruction.MSE > 3, "yes", "no")) %>%  
  arrange(Reconstruction.MSE) %>% 
  ggplot(aes(x = rowname, y = Reconstruction.MSE)) +
  geom_point(aes(color = outlier)) +
  geom_hline(aes(yintercept = 3), color = "blue") +
  scale_color_manual(values = c("darkgreen", "red"))

