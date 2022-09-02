# “purrr” paketi ----

library(tidyverse) #library(purrr)


# map() funksiyasından istifadə edərək for looplarının aradan qaldırılması ----

# map() funksiyası
kvadrat <- function(v){
  return(v * v)
}
vektor_1 <- c(2,3,4,5)
vektor_1 %>% map(kvadrat)


1:10 %>%
  map(rnorm, n = 10)

1:10 %>%
  map(rnorm, n = 10) %>%
  map_dbl(mean) 


# map2() funksiyası
üstü <- function(x, y){
  return(x**y)
}

x <- c(2, 4, 6, 8)
y <- c(3, 5, 7, 9)

map2(x, y, üstü)


# pmap() funksiyası
mtcars_sub <- mtcars[1:4,c("mpg", "cyl", "disp")]
mtcars_sub %>% pmap(sum)


# walk() funksiyası

mtcars_sub %>% 
  walk(print)

mtcars_sub %>% 
  walk(glimpse)


# Listlərin ümumiləşdirilməsi ----

list_2 <- list("dplyr", 3, "tibble", 9, "purrr")

list_2 %>% every(is.character)

list_2 %>% some(is.character)

list_2 %>% has_element(3)


# cross_df() funksiyası ----

df <- list(id = c(101, 102, 103, 104),
           ad = c("Abdullah", "Arif", "Adil", "Aqil"),
           yaş = c(19, 21, 23, 25))

df %>% cross_df()


# rerun() funksiyası ----

rerun(2, sample(6))

rerun(2, x = runif(1), y = runif(3))


# reduce() funksiyası ----

reduce(c(12,23,34,45,56,67,78,89,90), `+`)


# accumulate() funksiyası ----

accumulate(c(1,2,3,4,5), sum)

