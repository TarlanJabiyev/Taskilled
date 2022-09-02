library(tidyverse)
library(imbalance)

data <- imbalance::newthyroid1

data %>% glimpse()

data$Class %>% table() %>% prop.table()

data %>% imbalanceRatio("Class")

data$Class %>% table()

# MWMOTE (Majority Weighted Minority Oversampling TEchnique)
new_mwmote <- data %>% mwmote(numInstances = 75, classAttr = "Class")
new_mwmote$Class %>% table()

data %>% 
  plotComparison(rbind(data, new_mwmote), 
                 attrs = names(data)[1:3])

# RACOG (RApidly COnverging Gibbs)
new_RACOG <- data %>% racog(numInstances = 75, classAttr = "Class")
new_RACOG$Class %>% table()

data %>% 
  plotComparison(rbind(data, new_RACOG), 
                 attrs = names(data)[1:3])

# RWO (Random Walk Oversampling)
new_RWO <- data %>% mwmote(numInstances = 75, classAttr = "Class")
new_RWO$Class %>% table()

data %>% 
  plotComparison(rbind(data, new_RWO), 
                 attrs = names(data)[1:3])

# PdataOS (Probability Distribution density Function estimation based OverSampling)
new_PdataOS <- data %>% mwmote(numInstances = 75, classAttr = "Class")
new_PdataOS$Class %>% table()

data %>% 
  plotComparison(rbind(data, new_PdataOS), 
                 attrs = names(data)[1:3])

# oversample 
filtered2 <- data %>% 
  oversample(ratio = 1, method = "RACOG", # "MWMOTE","RACOG","RWO","PDFOS"
             filtering = TRUE, iterations = 500, classAttr = "Class")
filtered2$Class %>% table()

data %>% 
  plotComparison(filtered2, 
                 attrs = names(data)[1:3])
