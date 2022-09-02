# “forcats” paketi

library(tidyverse) #library(forcats)

data <- dplyr::starwars

data %>% glimpse()

data$eye_color <- data$eye_color %>% as.factor()

data$eye_color %>% levels()


# fct_count() funksiyası ----

data %>% 
  count(eye_color, sort = T)

data$eye_color %>% 
  fct_count(sort = TRUE) 


# fct_unique() funksiyası ----

data$eye_color %>% unique()

data$eye_color %>% fct_unique()


# fct_reorder() funksiyası ----

df <- tibble::tribble(
  ~rəng,      ~a, ~b,
  "göy",       1,  2,
  "yaşıl",     6,  2,
  "bənövşəyi", 3,  3,
  "qırmızı",   2,  3,
  "sarı",      5,  1
)
df$color <- df$rəng %>% as.factor()
df$color %>% fct_reorder(df$a, min)


# fct_explicit_na() funksiyası ----

f1 <- factor(c(1, 1, NA, NA,2, 2, NA,2, 1, 2, 2))
f2 <- f1 %>% fct_explicit_na(na_level = "boş")


# fct_drop() funksiyası ----

f3 <- factor(c("aa","bb"),c("aa","bb","cc")) 
f4 <- f3 %>% fct_drop() 


# fct_collapse() funksiyası ----

data$eye_color %>% 
  fct_collapse(qarışıq  = c("red, blue", "green, yellow", "blue-gray")) 


# fct_other() funksiyası ----

data$eye_color %>% 
  fct_other(keep = c("blue", "brown", "black"), other_level = "başqa")


# fct_recode() funksiyası ----

df$rəng %>% fct_recode(r1 = "göy", r2 = "yaşıl", r3 = "sarı",
                       r4 = "qırmızı", r5 = "bənövşəyi")

