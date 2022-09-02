library(tidyverse)
library(survival)
library(survminer)
library(tidyquant)
library(patchwork)

data <- read_csv("insurance.csv") 

data %>% glimpse()

data <- data %>%
  mutate(Churn = ifelse(Churn == "Yes", 1, 0)) %>%
  mutate_if(is.character, as_factor)


# Model

model <- survfit(Surv(tenure, Churn) ~ Contract, data = data)

p1 <- model %>% 
  ggsurvplot(conf.int = T,
             data = data)

p1$plot +
  theme_tq() +
  scale_fill_tq() +
  scale_color_tq() +
  labs(title = "Müştərinin sağ qalma qrafiki")


# Risk cədvəlini əlavə etmək

p2 <- model %>% 
  ggsurvplot(conf.int = T,
             data = data,
             risk.table = T)

p2_plot <- p2$plot +
  theme_tq() +
  scale_fill_tq() +
  scale_color_tq() +
  labs(title = "Müştərinin sağ qalma qrafiki")

p2_table <- p2$table +
  theme_tq() +
  scale_fill_tq() +
  scale_color_tq() +
  theme(panel.grid = element_blank())

p2_plot / p2_table + plot_layout(heights = c(2,1))


# Qruplaşdırmaq

p3 <- model %>% 
  ggsurvplot_facet(conf.int = T,
                   data = data,
                   facet.by = "gender",
                   nrow = 1)

p3 +
  theme_tq() +
  scale_fill_tq() +
  scale_color_tq() +
  labs(title = "Müştərinin cinsinə görə sağ qalma qrafiki")

