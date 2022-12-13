library(regclass)
library(arules)

## Decision Tree and Association Rules for 2020s Songs
spotify_2010s = read.csv("spotify_2010s.csv")

## Data Prep
spotify_2010s1 = spotify_2010s[,-c(1:4,8:10,24:25)]
spotify2 = spotify_2010s1
spotify2 = spotify2[complete.cases(spotify2),]

## Variable Discretization

# Popularity: Below 65 is Low, at or Above 65 is High
summary(spotify2$popularity)
class(spotify2$popularity)
summary(discretize(spotify2$popularity,breaks=2))
x <- spotify2$popularity
summary( cut(x,c(-1,65,100)))
x <-cut(x,c(-1,65,100))
levels(x) <- c("Low","High")
spotify2$popularity <- x

# duration_ms
summary(spotify2$duration_ms)
class(spotify2$duration_ms)
summary(discretize(spotify2$duration_ms,breaks=6))
x <- spotify2$duration_ms
summary( cut(x,c(0,160000,180000,200000,220000,240000,2400000)))
x <-cut(x,c(0,160000,180000,200000,220000,240000,2400000))
levels(x) <- c("<160000ms","160000 to 180000ms","180000 to 200000ms",
               "200000 to 220000ms","220000 to 240000ms",">240000ms")
spotify2$duration_ms <- x
plot(popularity ~ duration_ms, data=spotify2)

# explicit
summary(spotify2$explicit)
spotify2$explicit <- as.factor(spotify2$explicit)
plot(popularity ~ explicit, data=spotify2)

# danceability
summary(spotify2$danceability)
class(spotify2$danceability)
summary(discretize(spotify2$danceability,breaks=5))
x <- spotify2$danceability
summary( cut(x,c(-1,0.5,0.75,1)))
x <-cut(x,c(-1,0.5,0.75,1))
levels(x) <- c("0 to 0.5","0.5 to 0.75","0.75 to 1")
spotify2$danceability <- x
plot(popularity ~ danceability, data=spotify2)

# energy
summary(spotify2$energy)
class(spotify2$energy)
summary(discretize(spotify2$energy,breaks=5))
x <- spotify2$energy
summary( cut(x,c(-1,0.5,0.75,1)))
x <-cut(x,c(-1,0.5,0.75,1))
levels(x) <- c("0 to 0.5","0.5 to 0.75","0.75 to 1")
spotify2$energy <- x
plot(popularity ~ energy, data=spotify2)

# key
summary(spotify2$key)
class(spotify2$key)
summary(discretize(spotify2$key,breaks=5))
x <- spotify2$key
summary( cut(x,c(-1,1,4,7,9,11)))
x <-cut(x,c(-1,1,4,7,9,11))
levels(x) <- c("0 to 1","1 to 4","4 to 7","7 to 9","9 to 11")
spotify2$key <- x
plot(popularity ~ key, data=spotify2)

# loudness
summary(spotify2$loudness)
class(spotify2$loudness)
summary(discretize(spotify2$loudness,breaks=5))
x <- spotify2$loudness
summary( cut(x,c(-60,-10,-7,-5,2)))
x <-cut(x,c(-60,-10,-7,-5,2))
levels(x) <- c("-60 to -10","-10 to -7","-7 to -5","-5 to 2")
spotify2$loudness <- x
plot(popularity ~ loudness, data=spotify2)

# mode
summary(spotify2$mode)
class(spotify2$mode)
spotify2$mode = as.factor(spotify2$mode)
plot(popularity ~ mode, data=spotify2)

# speechiness
summary(spotify2$speechiness)
class(spotify2$speechiness)
summary(discretize(spotify2$speechiness,breaks=5))
x <- spotify2$speechiness
summary( cut(x,c(-1,0.05,0.1,0.2,1)))
x <-cut(x,c(-1,0.05,0.1,0.2,1))
levels(x) <- c("0 to 0.05","0.05 to 0.1","0.1 to 0.2","0.2 to 1")
spotify2$speechiness <- x
plot(popularity ~ speechiness, data=spotify2)

# acousticness
summary(spotify2$acousticness)
class(spotify2$acousticness)
summary(discretize(spotify2$acousticness,breaks=5))
x <- spotify2$acousticness
summary( cut(x,c(-1,0.05,0.2,0.5,1)))
x <-cut(x,c(-1,0.05,0.2,0.5,1))
levels(x) <- c("0 to 0.05","0.05 to 0.2","0.2 to 0.5","0.5 to 1")
spotify2$acousticness <- x
plot(popularity ~ acousticness, data=spotify2)

# instrumentalness
summary(spotify2$instrumentalness)
class(spotify2$instrumentalness)
summary(discretize(spotify2$instrumentalness,breaks=3))
x <- spotify2$instrumentalness
summary( cut(x,c(-1,0.00001,1)))
x <-cut(x,c(-1,0.00001,1))
levels(x) <- c("0 to 0.00001","0.00001 to 1")
spotify2$instrumentalness <- x
plot(popularity ~ instrumentalness, data=spotify2)

# liveness
summary(spotify2$liveness)
class(spotify2$liveness)
summary(discretize(spotify2$liveness,breaks=5))
x <- spotify2$liveness
summary( cut(x,c(-1,0.1,0.2,1)))
x <-cut(x,c(-1,0.1,0.2,1))
levels(x) <- c("0 to 0.1","0.1 to 0.2","0.2 to 1")
spotify2$liveness <- x
plot(popularity ~ liveness, data=spotify2)

# valence
summary(spotify2$valence)
class(spotify2$valence)
summary(discretize(spotify2$valence,breaks=5))
x <- spotify2$valence
summary( cut(x,c(-1,0.25,0.5,0.75,1)))
x <-cut(x,c(-1,0.25,0.5,0.75,1))
levels(x) <- c("0 to 0.25","0.25 to 0.5","0.5 to 0.75","0.75 to 1")
spotify2$valence <- x
plot(popularity ~ valence, data=spotify2)

# tempo
summary(spotify2$tempo)
class(spotify2$tempo)
summary(discretize(spotify2$tempo,breaks=5))
x <- spotify2$tempo
summary( cut(x,c(-1,100,115,130,150,230)))
x <-cut(x,c(-1,100,115,130,150,230))
levels(x) <- c("<100","100 to 115","115 to 130","130 to 150",">150")
spotify2$tempo <- x
plot(popularity ~ tempo, data=spotify2)

# time_signature
summary(spotify2$time_signature)
class(spotify2$time_signature)
summary(discretize(spotify2$time_signature,breaks=5))
x <- spotify2$time_signature
summary( cut(x,c(-1,3,4,5)))
x <-cut(x,c(-1,3,4,5))
levels(x) <- c("<4","4","5")
spotify2$time_signature <- x
plot(popularity ~ time_signature, data=spotify2)

# followers
summary(spotify2$followers)
class(spotify2$followers)
summary(discretize(spotify2$followers,breaks=5))
x <- spotify2$followers
summary( cut(x,c(-1,15000,100000,350000,2750000,100000000)))
x <-cut(x,c(-1,15000,100000,350000,2750000,100000000))
levels(x) <- c("<15,000","15,000 to 100,000","100,000 to 350,000",
               "350,000 to 2,750,000",">2,750,000")
spotify2$followers <- x
plot(popularity ~ followers, data=spotify2)

# artist_popularity
summary(spotify2$artist_popularity)
class(spotify2$artist_popularity)
summary(discretize(spotify2$artist_popularity,breaks=5))
x <- spotify2$artist_popularity
summary( cut(x,c(-1,50,60,70,80,100)))
x <-cut(x,c(-1,50,60,70,80,100))
levels(x) <- c("<50","50 to 60","60 to 70",
               "70 to 80",">80")
spotify2$artist_popularity <- x
plot(popularity ~ artist_popularity, data=spotify2)

# year
summary(spotify2$year)
class(spotify2$year)
spotify2$year = as.factor(spotify2$year)
plot(popularity ~ year, data=spotify2)

## DECISION TREE
#Combos
TREE <- rpart(popularity ~ .,data=spotify2,cp= 0.0097863)
TREE$cptable
summarize_tree(TREE)
visualize_model(TREE)

TREE1 <- rpart(popularity ~ .-artist_popularity,data=spotify2,cp= 0.0097863)
TREE1$cptable
summarize_tree(TREE1)
visualize_model(TREE1)
# artist popularity and followers tend to play the biggest role in determining
# popularity in the decision tree

## ASSOCIATION RULES

# Overall average # of songs that have high popularity (>65 score)
mean(spotify2$popularity=="High")

SPOTIFY.TRANS1 <- as(spotify2,"transactions")
itemFrequency(SPOTIFY.TRANS1)["popularity=High"]
#Consider pockets of size 200 or more
RULES1 <- apriori(SPOTIFY.TRANS1,parameter = list(supp=200/length(SPOTIFY.TRANS1),conf=0.50,maxlen=4),
                 appearance = list(default="lhs",rhs="popularity=High"), control=list(verbose=FALSE))
RULES1 <- RULES1[!is.redundant(RULES1)]
RULES1 <- RULES1[is.significant(RULES1,SPOTIFY.TRANS1)]
length(RULES1)
inspect(sort(RULES1,by="confidence"))
# similar results to decision tree; some other factors do show up in the
# association rules though, such as explicit, tempo, and liveness


library(ggplot2)
library(tidyverse)

ggplot(data=spotify_2010s1, aes(x=popularity)) + geom_histogram() + theme_bw()
ggplot(data=spotify_2010s1, aes(x=duration_ms)) + geom_histogram() + theme_bw()
ggplot(data=spotify_2010s1, aes(x=as.factor(year),y=popularity)) + geom_boxplot() + theme_bw()
summary_df1 <- spotify_2010s %>% 
  group_by(year) %>% 
  summarise(AvgPop = mean(popularity, na.rm = TRUE),
            AvgArtPop = mean(artist_popularity, na.rm = TRUE),
            AvgFollow = mean(followers, na.rm = TRUE),
            AvgTempo = mean(tempo, na.rm = TRUE),
            AvgDance = mean(danceability, na.rm = TRUE),
            AvgEnergy = mean(energy, na.rm = TRUE),
            AvgDur = mean(duration_ms, na.rm = TRUE),
            NumExp = sum(explicit),
            NumArtists = length(unique(id_artists)))

## Linear Regression

library(regclass)
popular <- lm(popularity ~ danceability + energy + speechiness + acousticness, data = spotify_2010s1)
summary(popular)
art_pop <- lm(artist_popularity ~ popularity + followers + explicit, data = spotify_2010s1)
summary(art_pop)































