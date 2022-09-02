# "tibble" paketi

library(tidyverse) #library(tibble)


# tibble'a giriş ----
starwars <- dplyr::starwars

starwars_df <- starwars %>% as.data.frame()

starwars %>% glimpse()


# tibble yaratmaq ----

tibble(
  ad = c("Luke Skywalker", "C-3PO", "R2-D2"),
  boy = c(172, 167, 96),
  çəki = c(77.0, 75.0, 32.0)
)

tibble_row(
  a = "model1", 
  b = "model2"
)

tribble(
  ~ad,            ~boy,    ~çəki,
  "Luke Skywalker", 172,     77.0,
  "C-3PO",          167,     75.0,
  "R2-D2",           96,     32.0
)

starwars_df %>%
  as_tibble() %>%
  class()


# tibble manipulyasiyası ----

starwars %>%
  slice(1:3) %>%
  add_row(
    name = "Tarlan",
    height = 175,
    mass = 64,
    skin_color = "ağ",
    eye_color = "qara",
    sex = "kişi",
    gender = "kişi",
    homeworld = "Earth"
  )

starwars %>%
  add_column(id = 1:nrow(.), .before = "name")


# Sətir adları ilə işləmək ----

starwars %>%
  select(name, height, mass) %>%
  column_to_rownames(var = "name") %>%
  head()

starwars %>%
  select(name, height, mass) %>%
  column_to_rownames(var = "name") %>%
  remove_rownames() %>%
  head()

starwars %>%
  select(name, height, mass) %>%
  column_to_rownames(var = "name") %>%
  rownames_to_column(var = "name") %>%
  head()

starwars %>%
  select(name, height, mass) %>%
  rowid_to_column(var = "id") %>%
  head()
