# Data təmizlənməsi və hazırlanması

library(tidyverse)


# Data tipinin düzəldilməsi ----

# character -> numerics
c <- c("12","13","14","12","12")
c %>% glimpse()

d <- c %>% as.numeric()
d %>% glimpse()

# factor -> numerics
f <- c("12","13","14","12","12") %>% as.factor()
f %>% glimpse()

i <- f %>% as.numeric()
i %>% glimpse()

# factor -> character -> numeric
n <- f %>% as.character() %>% as.numeric() 
n %>% glimpse()


# Date
x <- "2019-03-21"
y1 <- x %>% as.Date()
y1

x <- "2019-mar-21"
y2 <- x %>% as.Date("%Y-%b-%d")
y2

x <- "21-mar-19"
y3 <- x %>% as.Date("%d-%b-%y")
y3

x <- "21/MAR/19"
y4 <- x %>% as.Date("%d/%B/%y")
y4

x <- "3.21.2019"
y5 <- x %>% as.Date("%m.%d.%Y")
y5

identical(y1,y2,y3,y4,y5); y1; y2; y3; y4; y5


library(lubridate) 

ymd(20191123)
dmy(23112019)
mdy(11232019)

lubridate::year(y1)
lubridate::month(y2)
lubridate::day(y3)

interval("2017-11-01","2018-11-08") %>% as.period(unit = "year") %>% #month,day
  as.character() %>% substr(1,nchar(.)-9)


# NA’lərin doldurulması ----
library(data.table)
library(inspectdf)

df <- fread("startaplar.csv", na.strings = "")

df %>% glimpse()

df$ID <- df$ID %>% as.character()

# gsub() 
df$Expenses <- gsub(" Dollars","",df$Expenses)
df$Expenses <- gsub(",","",df$Expenses) %>% as.numeric()

# df$Expenses <- df$Expenses %>% 
#   gsub(" Dollars","",.) %>% 
#   gsub(",","",.) %>% 
#   as.numeric()

# parse_number()
df$Revenue <- df$Revenue %>% parse_number() 

# str_replace_all()
df$Growth <- df$Growth %>% str_replace_all("%","") %>% as.numeric()


df %>% glimpse()
df %>% summary()


# NA'lər
df %>% inspect_na()

df[!complete.cases(df),] %>% View()


df[df$Employees == 45,]
df[which(df$Employees == 45),]

df[df$Expenses == NA,]
df[is.na(df$Expenses),]


df[!is.na(df$Industry),]
df <- df[!is.na(df$Industry),]


df[is.na(df$State),]
df[is.na(df$State) & df$City=="New York",] 
df[is.na(df$State) & df$City=="New York","State"] <- "NY"

df[is.na(df$State),]
df[is.na(df$State) & df$City=="San Francisco",] 
df[is.na(df$State) & df$City=="San Francisco","State"] <- "CA"


med <- median(df[df$Industry=="Retail" & df$Inception=="2012","Employees"][[1]], na.rm=TRUE)
df[is.na(df$Employees) & df$Industry=="Retail" & df$Inception=="2012","Employees"] <- med

med <- median(df[df$Industry=="Financial Services" & df$Inception=="2010","Employees"][[1]], na.rm=TRUE)
df[is.na(df$Employees) & df$Industry=="Financial Services" & df$Inception=="2010","Employees"] <- med

med <- median(df[df$Industry=="Construction" & df$Inception=="2013","Growth"][[1]], na.rm=TRUE)
df[is.na(df$Growth) & df$Industry=="Construction" & df$Inception=="2013","Growth"] <- med

med <- median(df[df$Industry=="Construction" & df$Inception=="2010","Revenue"][[1]], na.rm=TRUE)
df[is.na(df$Revenue) & df$Industry=="Construction" & df$Inception=="2010","Revenue"] <- med

med <- median(df[df$Industry=="Construction" & df$Inception=="2013","Revenue"][[1]], na.rm=TRUE)
df[is.na(df$Revenue) & df$Industry=="Construction" & df$Inception=="2013","Revenue"] <- med

med <- median(df[df$Industry=="Construction" & df$Inception=="2010","Expenses"][[1]], na.rm=TRUE)
df[is.na(df$Expenses) & df$Industry=="Construction" & df$Inception=="2010","Expenses"] <- med

med <- median(df[df$Industry=="Construction" & df$Inception=="2013","Expenses"][[1]], na.rm=TRUE)
df[is.na(df$Expenses) & df$Industry=="Construction" & df$Inception=="2013","Expenses"] <- med


df[is.na(df$Profit),"Profit"] <- df[is.na(df$Profit),"Revenue"] - df[is.na(df$Profit),"Expenses"]
df[is.na(df$Expenses),"Expenses"] <- df[is.na(df$Expenses),"Revenue"] - df[is.na(df$Expenses),"Profit"]


df$Inception %>% glimpse()
df$Inception <- df$Inception %>% as.factor()

x <- df[which(df$Industry == "Health"),"Inception"][[1]]
x <- x[!is.na(x)]
ux <- unique(x)
mode_inc <- ux[match(x, ux) %>% tabulate() %>% which.max()]
df[is.na(df$Inception) & df$Industry == "Health","Inception"] <- mode_inc

df %>% inspect_na()

Mode <- function(x, na.rm = TRUE) {
  if(na.rm){
    x <- x[!is.na(x)]
  }
  ux <- unique(x)
  return(ux[match(x, ux) %>% tabulate() %>% which.max()])
}
Mode(x, na.rm = TRUE) -> df[is.na(df$Inception) & df$Industry == "Health","Inception"]


# Outlier'lər ----
library(graphics)

num_vars <- df %>% 
  select_if(is.numeric) %>% 
  names()
num_vars

for (b in num_vars) {
  OutVals <- boxplot(df[[b]])$out
  if(length(OutVals)>0){
    print(b)
    print(OutVals)
  }
}

OutVals <- boxplot(df[["Revenue"]])$out
median <- median(df[["Revenue"]])
o3 <- ifelse(OutVals>median,OutVals,NA) %>% na.omit() %>% as.matrix() %>% .[,1]
o1 <- ifelse(OutVals<median,OutVals,NA) %>% na.omit() %>% as.matrix() %>% .[,1]

val <- quantile(df[["Revenue"]], 0.75, na.rm = T) + 1.5 * IQR(df[["Revenue"]], na.rm = T)
df[df[["Revenue"]] %in% o3,"Revenue"] <- val

val <- quantile(df[["Revenue"]], 0.25, na.rm = T) - 1.5 * IQR(df[["Revenue"]], na.rm = T)
df[df[["Revenue"]] %in% o1,"Revenue"] <- val


OutVals <- boxplot(df[["Employees"]])$out
median <- median(df[["Employees"]])
o3 <- ifelse(OutVals>median,OutVals,NA) %>% na.omit() %>% as.matrix() %>% .[,1]
o1 <- ifelse(OutVals<median,OutVals,NA) %>% na.omit() %>% as.matrix() %>% .[,1]

val <- quantile(df[["Employees"]], 0.75, na.rm = T) + 1.5 * IQR(df[["Employees"]], na.rm = T)
df[df[["Employees"]] %in% o3,"Employees"] <- val

val <- quantile(df[["Employees"]], 0.25, na.rm = T) - 1.5 * IQR(df[["Employees"]], na.rm = T)
df[df[["Employees"]] %in% o1,"Employees"] <- val


OutVals <- boxplot(df[["Profit"]])$out
median <- median(df[["Profit"]])
o3 <- ifelse(OutVals>median,OutVals,NA) %>% na.omit() %>% as.matrix() %>% .[,1]
o1 <- ifelse(OutVals<median,OutVals,NA) %>% na.omit() %>% as.matrix() %>% .[,1]

val <- quantile(df[["Profit"]], 0.75, na.rm = T) + 1.5*IQR(df[["Profit"]], na.rm = T)
df[df[["Profit"]] %in% o3,"Profit"] <- val

val <- quantile(df[["Profit"]], 0.25, na.rm = T) - 1.5*IQR(df[["Profit"]], na.rm = T)
df[df[["Profit"]] %in% o1,"Profit"] <- val


# Outlier'lər üçün funksiya yaratmaq
for_vars <- c()
for (b in 1:length(num_vars)) {
  OutVals <- boxplot(df[[num_vars[b]]], plot=F)$out
  if(length(OutVals)>0){
    for_vars[b] <- num_vars[b]
  }
}
for_vars <- for_vars %>% as.data.frame() %>% drop_na() %>% pull(.)
for_vars %>% length()

for (o in for_vars) {
  OutVals <- boxplot(df[[o]], plot=F)$out
  mean <- mean(df[[o]],na.rm=T)
  
  o3 <- ifelse(OutVals>mean,OutVals,NA) %>% na.omit() %>% as.matrix() %>% .[,1]
  o1 <- ifelse(OutVals<mean,OutVals,NA) %>% na.omit() %>% as.matrix() %>% .[,1]
  
  val3 <- quantile(df[[o]], 0.75, na.rm = T) + 1.5 * IQR(df[[o]], na.rm = T)
  df[which(df[[o]] %in% o3),o] <- val3
  
  val1 <- quantile(df[[o]], 0.25, na.rm = T) - 1.5*IQR(df[[o]], na.rm = T)
  df[which(df[[o]] %in% o1),o] <- val1
}

