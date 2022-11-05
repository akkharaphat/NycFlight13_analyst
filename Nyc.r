# -*- coding: utf-8 -*-

# -- Sheet --

library(tidyverse)
nyc<- read_csv("flights.csv")
airport <- read_csv("airport.csv")
airline <- read_csv("airline.csv")    

nyc %>% glimpse()
glimpse(airport)
glimpse(airline)

# How many distance in 2013 and which the most distance flight
# 
# In **August** is most distance in 2013 **(50150427.74)**
#  , **Flight 51** is Longest distance **(4983 Mile)**
#  and **Flight 461** is Lowest distance **(80 mile)**


nyc %>%
  select(month,distance) %>%
  group_by(month) %>%
  summarise(Km. = sum(distance)*1.61)
    

nyc %>%
    group_by(flight) %>%
    summarise(distance = max(distance)) %>%
    arrange(desc(distance)) %>%
    head(5)
    
    

nyc %>%
    group_by(flight) %>%
    summarise(distance = max(distance,rm.na = T)) %>%
    arrange(desc(distance)) %>%
    tail(5)

# # Which carrier in **May** is fly the most
# **United Air Lines Inc. 4960 **


nyc %>% 
    filter(month == 5) %>%
    count(carrier) %>%
    arrange(desc(n)) %>%
    left_join(airline,by ="carrier") %>%
    head(5)

# # where the lowest destination
# **La Guardia Airport and Blue Grass airport**


nyc %>%
    count(dest) %>%
    arrange(desc(n)) %>%
    left_join(airport,by = c("dest" = "faa")) %>%
    tail(5)

airport <- subset(airport,select = -c(1))

# # Which month has the most flights in Miami Intl?
# #December


nyc %>%
    filter(dest == "MIA") %>%
    group_by(month) %>%
    count(dest) %>%
    arrange(desc(n)) %>%
    left_join(airport,by = c("dest" = "faa"))


# # JFK longest and lowest flights distance in 2013
# #Longest 4983 mile 
# #Lowest   94 mile


nyc %>%
    group_by(origin) %>%
    filter(origin == "JFK") %>%
    summarise(Longest_dist = max(distance),
              Lowest_dist  = min(distance))

# # Which airline has the least delays in JFK
# # HA : Hawaiian Airlines Inc.


nyc %>%
    group_by(carrier)%>%
    filter(origin == "JFK") %>%
    summarize(avg_delay = mean(arr_delay, na.rm = T)) %>%
            arrange(avg_delay) %>%
            left_join(airline,by = "carrier")
    

install.packages("RPostgreSQL")
library(RPostgreSQL)

# connect data base
con <- dbConnect(
  PostgreSQL(),
  host = "tiny.db.elephantsql.com",
  dbname = "sxkiwtvf",
  port = 5432,
  user = "sxkiwtvf",
  password = "j-Hpowam7_AYVhDHDDC-BzfoknQjan4-"
)

# create dataframe
fav_game <- data.frame(
    game_num = 1:5,
    game_name = c("Counter-Strike: Global Offensive", "	Dota 2", "	Apex Legends", "Grand Theft Auto V", "Rust"),
    Dev = c("Valve", "Bizzard", "Respawn Entertainment" , "Rockstar", "Facepunch")
)

# create table in database
dbWriteTable(con, "fav_game", fav_game)


fav_place <- data.frame(
    id = 1:5
    country = c("China", "Korea", "Laos", "Zombia", "Belgium" ),
    region = c("Asia", "Asia", "Asia", "Africa", "Europe")

) 
    
    # create table in database
dbWriteTable(con, "fav_place", fav_place)

movie_list <- data.frame(
  id = 1:5,
  movie_name = c("Lost", "The Midnight Guy", "Top Gun: Maverick", "Knives Out", "La La Land"),
  score = c(3, 2, 5, 3, 4)
  year = c("2010", "2016", "2022", "2017", "2016")
)

dbWriteTable(con, "books_list", books_list)

# list tables in database
dbListTables(con)

#Query
dbGetQuery(con, "SELECT * FROM fav_place")

# disconnect database
dbDisconnect(con)

