library(tidyverse)
library(stringr)
library(lubridate)

## Loading in Data
spotify = read.csv("https://media.githubusercontent.com/media/Bert4214/BZAN-542-Project/main/Spotify_Songs_NoNA.csv", sep="\t")

## General Data Description
table(spotify$genres)
min(spotify$release_date)
max(spotify$release_date)

## Working with Dates
spotify$year = substr(spotify$release_date,1,4)
class(spotify$year)
spotify$year = as.integer(spotify$year)
spotify = spotify %>% 
  filter(year >= 1950)
summary(spotify$year)

## Subsetting by Decade
spotify_1950s = spotify %>% 
  filter(year >= 1950 & year < 1960)
write.csv(spotify_1950s,"spotify_1950s.csv")

spotify_1960s = spotify %>% 
  filter(year >= 1960 & year < 1970)
write.csv(spotify_1960s,"spotify_1960s.csv")

spotify_1970s = spotify %>% 
  filter(year >= 1970 & year < 1980)
write.csv(spotify_1970s,"spotify_1970s.csv")

spotify_1980s = spotify %>% 
  filter(year >= 1980 & year < 1990)
write.csv(spotify_1980s,"spotify_1980s.csv")

spotify_1990s = spotify %>% 
  filter(year >= 1990 & year < 2000)
write.csv(spotify_1990s,"spotify_1990s.csv")

spotify_2000s = spotify %>% 
  filter(year >= 2000 & year < 2010)
write.csv(spotify_2000s,"spotify_2000s.csv")

spotify_2010s = spotify %>% 
  filter(year >= 2010 & year < 2020)
write.csv(spotify_2010s,"spotify_2010s.csv")

spotify_2020s = spotify %>% 
  filter(year >= 2020)
write.csv(spotify_2020s,"spotify_2020s.csv")

