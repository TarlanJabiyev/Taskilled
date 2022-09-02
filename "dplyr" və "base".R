# “dplyr” və "base" paketləri

library(dplyr)


# Dəyişənlərə görə sıraları düzmək ----

# dplyr
mtcars %>% arrange(cyl, hp)

mtcars %>% arrange(desc(cyl), desc(hp))

# base
mtcars[order(mtcars$cyl, mtcars$hp),] 

mtcars[order(mtcars$cyl, mtcars$hp, decreasing = TRUE), ]
mtcars[order(-mtcars$cyl, -mtcars$hp), ]


# Unikal sıraları saxlamaq ----

df <- tibble(
  x = sample(10, 100, rep = TRUE),
  y = sample(10, 100, rep = TRUE)
)

# dplyr
df %>% distinct(x) 
df %>% distinct(x, .keep_all = T) 

# base
unique(df["x"]) 
df[!duplicated(df$x), ] 


# Uyğun şərtlərlə sıraları qaytarmaq ----

# dplyr
starwars %>% filter(species == "Human")
starwars %>% filter(mass > 1000)
starwars %>% filter(hair_color == "none" & eye_color == "black")

# base 
subset(starwars, species == "Human")
subset(starwars, mass > 1000)
subset(starwars, hair_color == "none" & eye_color == "black")

# base 
starwars[which(starwars$species == "Human"), ]
starwars[which(starwars$mass > 1000), ]
starwars[which(starwars$hair_color == "none" & starwars$eye_color == "black"), ]


# Yeni dəyişən yaratmaq ----

df <- tibble(
  x = sample(10, 100, rep = TRUE),
  y = sample(10, 100, rep = TRUE)
)


# dplyr
df %>% 
  mutate(z_1 = x + y, 
         z_2 = z_1 ^ 2)

# base
df %>% 
  transform(z_1 = x + y, 
            z_2 = (x + y) ^ 2) 

# base
mtcars$cyl2 <- mtcars$cyl * 2
mtcars$cyl4 <- mtcars$cyl2 * 2


# dplyr
gf <- tibble(g = c(1, 1, 2, 2), 
             x = c(0.5, 1.5, 2.5, 3.5))
gf %>% 
  group_by(g) %>% 
  mutate(x_mean = mean(x), 
         x_max = max(x))

# base
transform(gf, 
          x_mean = ave(x, g, FUN = mean), 
          x_rank = ave(x, g, FUN = max))


# Tək dəyişəni çıxarmaq ----

# dplyr
mtcars %>% pull(1)
mtcars %>% pull(cyl)

# base
mtcars[[1]]
mtcars$cyl


# Sütun sırasını dəyişdirmək ----

# dplyr 
mtcars %>% relocate(hp, cyl) 
mtcars %>% relocate(hp, cyl, .after = last_col()) 

# base
mtcars[union(c("hp", "cyl"), names(mtcars))] 
mtcars[c(setdiff(names(mtcars), c("hp", "cyl")), c("hp", "cyl"))] 


# Dəyişən adını dəyişmək ----

# dplyr
iris %>% rename(sepal_length = Sepal.Length, sepal_width = 2)

# base
iris2 <- iris
names(iris2)[2] <- "sepal_width"

# base
names(iris2)[names(iris2) == "Sepal.Length"] <- "sepal_length"


# Dəyişən adlarını funksiya ilə dəyişmək ----

# dplyr
iris %>% rename_with(toupper)
iris %>% rename_with(tolower)

# base
setNames(iris, toupper(names(iris)))
setNames(iris, tolower(names(iris)))


# Adına görə dəyişən seçmək ----

# dplyr 
iris %>% select(1:3)
iris %>% select(Species, Sepal.Length)
iris %>% select(starts_with("Petal"))
iris %>% select(where(is.factor))

# base
iris[1:3] 
iris[,1:3]

# base
iris[c("Species", "Sepal.Length")]
subset(iris, select = c(Species, Sepal.Length))

# base
iris[grep("^Petal", names(iris))]


# Birdən çox dəyəri ümumiləşdirərək bir dəyərə endirmək ----

# dplyr
mtcars %>% 
  group_by(cyl) %>% 
  summarise(mean = mean(hp), 
            n = n())

# base
mtcars_by <- by(
  mtcars, mtcars$cyl, function(df) {
  with(df, data.frame(cyl = cyl[[1]], 
                      mean = mean(hp), 
                      n = nrow(df)))}
  )
do.call(rbind, mtcars_by)


# Mövqeyinə görə sıra seçmək ----

# dplyr
mtcars %>% slice(7)
mtcars %>% slice(7:n())

# base
mtcars[7, ]
mtcars[7:nrow(mtcars), ]


# Dataları birləşdirmək ----

band_members
band_instruments
band_instruments2


band_members %>% 
  inner_join(band_instruments, 
             by = "name") # dplyr

band_members %>% 
  merge(band_instruments, by = "name") # base


band_members %>% 
  left_join(band_instruments, 
            by = "name") # dplyr

band_members %>% 
  merge(band_instruments, 
        by = "name",
        all.x = T) # base


band_members %>% 
  right_join(band_instruments, 
             by = "name") # dplyr

band_members %>% 
  merge(band_instruments, 
        by = "name",
        all.y = T) # base


band_members %>% 
  full_join(band_instruments, 
            by = "name") # dplyr

band_members %>% 
  merge(band_instruments, 
        by = "name",
        all = T) # base


band_members %>% 
  left_join(band_instruments2, 
            by = c("name" = "artist")) # dplyr

band_members %>% 
  merge(band_instruments2, 
        by.x = "name", by.y = "artist",
        all.x = T) # base
