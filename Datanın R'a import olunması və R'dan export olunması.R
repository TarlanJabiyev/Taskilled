# Datanın R’a import olunması və R’dan export olunması

# Kitabxanalar 
library(tidyverse)
library(dslabs)
library(rstudioapi)
library(writexl) 
library(readxl)
library(fs)


data <- gapminder 


# Working directory'ni dəyişmək ----
path <- dirname(getSourceEditorContext()$path)
setwd(path)

getwd()

# ctrl + shift + H + Desktop(məsələn)


# Datanı write funksiyaları ilə export etmək ----
data %>% write_csv('gapminder.csv')
data %>% write_xlsx('gapminder.xlsx')


# Datanı read funksiyaları ilə import etmək ----
df_csv <- read_csv("gapminder.csv")
df_xlsx <- read_xlsx("gapminder.xlsx")


# Datada hər qrupu ayrı-ayrı export etmək ----
data %>%
  group_by(continent) %>%
  group_split() %>%
  walk(function(x) {
    write_csv(x, path = str_c("./data/", unique(x$continent), ".csv"))
  })


# Çox faylları import etmək ----
file_paths <- dir_ls("data")

file_contents <- list()

for (i in seq_along(file_paths)) {
  file_contents[[i]] <- file_paths[[i]] %>% 
    read_csv()
}

file_contents <- file_contents %>% set_names(file_paths)


# Fərqli üsullarla R'a data import etmək ----

file <- read_csv(file.choose())

