# "dplyr" paketi

library(tidyverse) #library(dplyr)

data <- dslabs::gapminder


# %>% (control+shift+m) - pipe operatoru ----

dim(data)
head(data)
tail(data)
summary(data)
glimpse(data)
names(data)  

data %>% dim()
data %>% head()
data %>% tail()
data %>% summary()
data %>% glimpse()
data %>% names()

View(head(data,19),"ilk_19")
data %>% head(19) %>% View("ilk_19")


c(rep(paste0(seq(round(abs(sum(-3.14,-6))),5),"a"),3),"!")

-3.14 %>% 
  sum(-6) %>% 
  abs() %>% 
  round() %>% 
  seq(5) %>% 
  paste0("a") %>% 
  rep(3) %>% 
  c("!")


# arrange() funksiyası ----

data %>% 
  arrange(life_expectancy) %>% 
  head()

data %>% 
  arrange(desc(life_expectancy)) %>% 
  head()

data %>% 
  arrange(continent) %>% 
  head()

data %>% 
  arrange(continent,
          desc(life_expectancy)) %>% View()
  head()


# rename() funksiyası ----

data %>% 
  rename(ölkə = country,
         il = year) %>% 
  head()

data_2 <- data %>% 
  rename(ölkə = country,
         il = year)


# select() funksiyası ----

data %>% 
  select(country, year) %>% 
  head()

data %>% 
  select(-country, -year) %>% 
  head()

data %>% 
  select(country:population) %>% 
  head()

data %>% 
  select(1:6) %>% 
  head()

data %>% 
  select(!country:population) %>% 
  head()

data %>% 
  select(region,population,everything()) %>% 
  head()

data %>% 
  select(region,population,everything(),-country) %>% 
  head()

data %>% 
  select(contains("_")) %>% 
  head()

data %>% 
  select(starts_with("co")) %>% 
  head()

data %>% 
  select(ends_with("ty")) %>% 
  head()

data %>% 
  select(ölkə = country, il = year) %>% 
  head()


# relocate() funksiyası ----

data %>% 
  relocate(country, .after = year) %>% 
  head()

data %>% 
  relocate(country, .after = last_col()) %>% 
  head()

data %>% 
  relocate(country, .after = last_col(offset = 1)) %>% 
  head()

data %>% 
  relocate(ends_with("ty"), .before = country) %>% 
  head()


# filter() funksiyası ----

data %>%
  filter(year > 2009) 

data %>%
  filter(continent == "Europe")


data %>%
  filter(year > 2009,
         continent == "Europe") %>% 
  select(life_expectancy, gdp)

data %>% 
  filter(year %in% c(2001,2005,2009,2013),
         !continent %in% c("Europe","Asia"))


# pull() funksiyası ----

data %>% 
  filter(life_expectancy > 80) %>% 
  select(country) 

data %>% 
  filter(life_expectancy > 80) %>% 
  pull(country) 


# slice() funksiyası ----

data %>% 
  slice(1)

data %>% 
  slice(3:5)


data %>% 
  slice_sample(n = 5)


data %>% 
  slice_head(n = 7)

data %>% 
  slice_tail(prop = 0.01) %>% 
  nrow()
nrow(data) * 0.01


data %>% 
  slice_max(life_expectancy, n = 5)

data %>% 
  slice_min(life_expectancy, n = 5)


# distinct() funksiyası ----

data %>% 
  nrow()

data %>% 
  distinct() %>% 
  nrow()


data %>% 
  select(continent, region) %>% 
  distinct()

data %>% 
  select(country) %>% 
  distinct() %>% 
  pull()


# mutate() funksiyası ----

data %>% 
  mutate(yeni_sütun = life_expectancy / 100) %>% 
  head()

data %>% 
  mutate(life_expectancy = round(life_expectancy / 100, 2)) %>% 
  head()

data %>% 
  mutate(yeni_sütun = round(life_expectancy / 100, 2),
         yeni_sütun_2 = paste0(country,"_",continent,"_",region)) %>% 
  select(contains("yeni")) %>% 
  head()


# group_by() funksiyası ----

data %>% 
  group_by(continent) %>% 
  slice(1)

data %>% 
  group_by(continent, region) %>% 
  slice(1)


# summarise() funksiyası ----

data %>% 
  summarise(max(life_expectancy))

data %>% 
  summarise(max(life_expectancy),
            mean(life_expectancy),
            min(life_expectancy))

data %>% 
  summarise(max = max(life_expectancy) %>% round(1),
            ortalama = mean(life_expectancy) %>% round(1),
            min = min(life_expectancy) %>% round(1))


data %>% 
  group_by(country) %>%
  summarise(max(life_expectancy))

data %>% 
  group_by(country) %>%
  summarise(max(life_expectancy),
            mean(life_expectancy),
            min(life_expectancy))

data %>% 
  group_by(country) %>% 
  summarise(max_life_ex = max(life_expectancy)) %>% 
  arrange(desc(max_life_ex))

data %>% 
  group_by(continent, region) %>% 
  summarise(max_life_ex = max(life_expectancy)) %>% 
  arrange(continent,
          desc(max_life_ex))

data %>% 
  summarise(sum_gdp = sum(gdp, na.rm = T),
            mean_gdp = mean(gdp, na.rm = T),
            med_gdp = median(gdp, na.rm = T)) 
  

# count() və n() funksiyaları ----

data %>%
  count(continent, sort = T)

data %>% 
  group_by(continent) %>% 
  summarise(n = n())


data %>% 
  count(continent,region, sort = T)

data %>% 
  group_by(continent, region) %>% 
  summarise(n = n()) %>% 
  arrange(desc(n))


# “join” funksiyaları ----

band_members
band_instruments
band_instruments2


band_members %>% 
  inner_join(band_instruments, 
             by = "name")

band_members %>% 
  left_join(band_instruments, 
             by = "name")

band_members %>% 
  right_join(band_instruments, 
            by = "name")

band_members %>% 
  full_join(band_instruments, 
             by = "name")


band_members %>% 
  left_join(band_instruments2, 
            by = c("name" = "artist"))

band_members %>%
  left_join(band_instruments2, 
            by = c("name" = "artist"), 
            keep = T)
