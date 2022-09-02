# "tidyr" paketi

library(tidyverse) #library(tidyr)

df <- data.frame(
  S.No = 1:10, 
  qrup.1 = c(12, 23, 34, 45, 56, 67, 78, 89, 90, 1),
  qrup.2 = c(123, 234, 345, 456, 567, 678, 789, 890, 901, 12),
  qrup.3 = c(1234, 2345, 3456, 4567, 5678, 6789, 7890, 8901, 9012, 123))


# gather() funksiyası ----

df %>% 
  gather(key = qrup, value = dəyər,
         qrup.1:qrup.3)

gather_df <- df %>% 
  gather(qrup, dəyər,
         qrup.1:qrup.3)
gather_df


# separate() funksiyası ----

separate_df <- gather_df %>% 
  separate(qrup, c("hissə", "nömrə")#, sep = "\\."
           )

separate_df


# unite() funksiyası ----

unite_df <- separate_df %>%
  unite(qrup, hissə, 
        nömrə, sep = ".")


# spread() funksiyası ----

əvvəlki_hal <- unite_df %>% 
  spread(qrup, dəyər)


# pivot_longer() funksiyası ----

relig_income

relig_income %>%
  pivot_longer(!religion, names_to = "gəlir", values_to = "say")

relig_income %>% 
  gather(gəlir, say,
         -religion)


# pivot_wider() funksiyası ----

fish_encounters

fish_encounters %>%
  pivot_wider(names_from = station, values_from = seen)

fish_encounters %>% 
  spread(station, seen)
