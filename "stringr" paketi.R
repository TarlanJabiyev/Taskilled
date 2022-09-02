# “stringr” paketi

library(tidyverse) #library(stringr)


# str_c() funksiyası ----
x <- c("salam", "əziz", "izləyicilər", "dostlar")

x %>% paste(collapse = " ")

x %>% str_c(collapse = " ") 


# str_length() funksiyası ----
x %>% str_length()


# str_count() funksiyası ----

y <- x %>% str_c(collapse = " ")


y %>% regmatches(gregexpr("a", y)) %>% lengths()

y %>% str_count("a")


# str_sub() funksiyası ----

y %>% substr(1, 3) 


y %>% substr(nchar(y) - 3 + 1, nchar(y))

y %>% str_sub(-3, -1) 


# str_replace_all() funksiyası ----

y %>% str_replace_all(" ", "_")

y %>% str_replace_all(" ", "")


# str_detect() funksiyası ----

x %>% str_detect("a")

x[x %>% str_detect("a")]


# str_extract() funksiyası ----

x %>% str_extract("la")



