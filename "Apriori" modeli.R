library(tidyverse)
library(rstudioapi)
library(arules)
library(arulesViz)

path <- dirname(getSourceEditorContext()$path)
setwd(path)

data <- read_csv("Market_Basket_Optimisation.csv",col_names = F)


# "Apriori" modelinin qurulması ----
df <- read.transactions("Market_Basket_Optimisation.csv",
                        sep = ",", rm.duplicates = T)

df %>% summary()

df %>% itemFrequencyPlot(topN = 12, type = "absolute")

qaydalar <- df %>% apriori(parameter = list(support = 0.01, confidence = 0.2))


# Əldə olunan qaydaların vizuallaşdırılması ----

qaydalar %>% inspect() # 164 qayda
#lhs - left hand side ; rhs - right hand side 

qaydalar %>% sort(by = "support") %>% 
  .[1:12] %>% inspect() 
#Bir əlaqənin bütün alışverişlər içində hansı nisbətdə təkrarlandığını bildirir

qaydalar %>% sort(by = "confidence") %>% 
  .[1:12] %>% inspect() 
#X məhsulunu alan müştərilərin Y məhsulunu alma ehtimalını bildirir

qaydalar %>% sort(by = "lift") %>% 
  .[1:12] %>% inspect() 
#Əlaqənin qəribəliliyini/fərqliliyini bildirir

qaydalar %>% sort(by = "support") %>% .[1:12] %>% arulesViz::inspectDT()

qaydalar %>% sort(by = "support") %>% .[1:12] %>% 
  plot(method = "graph",  engine = "htmlwidget")

