# SQL'dən R'a

# R Studio’da databazaya qoşulmaq ----

library(tidyverse)
library(rstudioapi)
library(RSQLite)

path <- dirname(getSourceEditorContext()$path)
setwd(path)

con <- dbConnect(SQLite(),"bikes_database.db")

con %>% dbListTables()

velosipedlər <- con %>% tbl("bikes") %>% collect()
mağazalar <- con %>% tbl("bikeshops") %>% collect()
sifariş_xətləri <- con %>% tbl("orderlines") %>% collect()

con %>% dbDisconnect()

# MySQL databazasına qoşulmaq
con <- dbConnect(RMySQL::MySQL(), 
                 dbname = "tweater", 
                 host = "courses.csrrinzqubik.us-east-1.rds.amazonaws.com", 
                 port = 3306,
                 user = "student",
                 password = "datacamp")

con %>% dbListTables()

comments <- con %>% tbl("comments") %>% collect()
tweats <- con %>% tbl("tweats") %>% collect()

con %>% dbDisconnect()


# R kodu SQL koduna çevirmək ----

library(tidyverse)
library(dslabs)
library(dbplyr)

db_gapminder <- gapminder %>% tbl_memdb(name = "db_gapminder")

db_gapminder %>% 
  select(country,life_expectancy,gdp) %>% 
  show_query()

db_gapminder %>% 
  select(country,life_expectancy,gdp) %>%
  rename(life.exp = life_expectancy) %>% 
  show_query()

db_gapminder %>% 
  select(country,life_expectancy,gdp) %>%
  rename(life.exp = life_expectancy) %>% 
  filter(life_expectancy > 50) %>% 
  show_query()

db_gapminder %>% 
  select(country,life_expectancy,gdp) %>%
  rename(life.exp = life_expectancy) %>% 
  filter(life_expectancy > 50,
         !is.na(gdp)) %>% 
  show_query()

db_gapminder %>% 
  select(country,life_expectancy,gdp) %>%
  rename(life.exp = life_expectancy) %>% 
  filter(life_expectancy > 50,
         !is.na(gdp)) %>% 
  group_by(country) %>%
  summarise(life_expectancy = max(life.exp, na.rm=T)) %>% 
  show_query()


# SQL kodu R koduna çevirmək ----

library(tidyquery)
library(nycflights13)

show_dplyr("SELECT Species, COUNT(*) AS n 
            FROM iris 
            GROUP BY Species")

show_dplyr("SELECT origin, dest, COUNT(*) AS num_flights, SUM(seats) AS num_seats
            FROM flights f 
            LEFT JOIN planes p
            ON f.tailnum = p.tailnum
            WHERE distance BETWEEN 250 AND 450
            GROUP BY origin,dest
            HAVING num_flights > 200
            ORDER BY num_seats DESC
            LIMIT 8")
