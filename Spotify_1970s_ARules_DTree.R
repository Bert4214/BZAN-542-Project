library(regclass)
library(arules)

## Decision Tree and Association Rules for 1970s Songs
spotify_1970s = read.csv("spotify_1970s.csv")

## Data Prep
spotify_1970s1 = spotify_1970s[,-c(1:4,8:10,24:25)]
spotify1 = spotify_1970s1
spotify1 = spotify1[complete.cases(spotify1),]

## Variable Discretization

# Popularity: Below 65 is Low, at or Above 65 is High
summary(spotify1$popularity)
class(spotify1$popularity)
summary(discretize(spotify1$popularity,breaks=2))
x <- spotify1$popularity
summary( cut(x,c(-1,65,100)))
x <-cut(x,c(-1,65,100))
levels(x) <- c("Low","High")
spotify1$popularity <- x

# duration_ms
summary(spotify1$duration_ms)
class(spotify1$duration_ms)
summary(discretize(spotify1$duration_ms,breaks=6))
x <- spotify1$duration_ms
summary( cut(x,c(0,160000,180000,200000,220000,240000,2400000)))
x <-cut(x,c(0,160000,180000,200000,220000,240000,2400000))
levels(x) <- c("<160000ms","160000 to 180000ms","180000 to 200000ms",
               "200000 to 220000ms","220000 to 240000ms",">240000ms")
spotify1$duration_ms <- x
plot(popularity ~ duration_ms, data=spotify1)

# explicit
summary(spotify1$explicit)
spotify1$explicit <- as.factor(spotify1$explicit)
plot(popularity ~ explicit, data=spotify1)

# danceability
summary(spotify1$danceability)
class(spotify1$danceability)
summary(discretize(spotify1$danceability,breaks=5))
x <- spotify1$danceability
summary( cut(x,c(-1,0.5,0.75,1)))
x <-cut(x,c(-1,0.5,0.75,1))
levels(x) <- c("0 to 0.5","0.5 to 0.75","0.75 to 1")
spotify1$danceability <- x
plot(popularity ~ danceability, data=spotify1)

# energy
summary(spotify1$energy)
class(spotify1$energy)
summary(discretize(spotify1$energy,breaks=5))
x <- spotify1$energy
summary( cut(x,c(-1,0.5,0.75,1)))
x <-cut(x,c(-1,0.5,0.75,1))
levels(x) <- c("0 to 0.5","0.5 to 0.75","0.75 to 1")
spotify1$energy <- x
plot(popularity ~ energy, data=spotify1)

# key
summary(spotify1$key)
class(spotify1$key)
summary(discretize(spotify1$key,breaks=5))
x <- spotify1$key
summary( cut(x,c(-1,1,4,7,9,11)))
x <-cut(x,c(-1,1,4,7,9,11))
levels(x) <- c("0 to 1","1 to 4","4 to 7","7 to 9","9 to 11")
spotify1$key <- x
plot(popularity ~ key, data=spotify1)

# loudness
summary(spotify1$loudness)
class(spotify1$loudness)
summary(discretize(spotify1$loudness,breaks=5))
x <- spotify1$loudness
summary( cut(x,c(-60,-10,-7,-5,2)))
x <-cut(x,c(-60,-10,-7,-5,2))
levels(x) <- c("-60 to -10","-10 to -7","-7 to -5","-5 to 2")
spotify1$loudness <- x
plot(popularity ~ loudness, data=spotify1)

# mode
summary(spotify1$mode)
class(spotify1$mode)
spotify1$mode = as.factor(spotify1$mode)
plot(popularity ~ mode, data=spotify1)

# speechiness
summary(spotify1$speechiness)
class(spotify1$speechiness)
summary(discretize(spotify1$speechiness,breaks=5))
x <- spotify1$speechiness
summary( cut(x,c(-1,0.05,0.1,0.2,1)))
x <-cut(x,c(-1,0.05,0.1,0.2,1))
levels(x) <- c("0 to 0.05","0.05 to 0.1","0.1 to 0.2","0.2 to 1")
spotify1$speechiness <- x
plot(popularity ~ speechiness, data=spotify1)

# acousticness
summary(spotify1$acousticness)
class(spotify1$acousticness)
summary(discretize(spotify1$acousticness,breaks=5))
x <- spotify1$acousticness
summary( cut(x,c(-1,0.05,0.2,0.5,1)))
x <-cut(x,c(-1,0.05,0.2,0.5,1))
levels(x) <- c("0 to 0.05","0.05 to 0.2","0.2 to 0.5","0.5 to 1")
spotify1$acousticness <- x
plot(popularity ~ acousticness, data=spotify1)

# instrumentalness
summary(spotify1$instrumentalness)
class(spotify1$instrumentalness)
summary(discretize(spotify1$instrumentalness,breaks=3))
x <- spotify1$instrumentalness
summary( cut(x,c(-1,0.00001,1)))
x <-cut(x,c(-1,0.00001,1))
levels(x) <- c("0 to 0.00001","0.00001 to 1")
spotify1$instrumentalness <- x
plot(popularity ~ instrumentalness, data=spotify1)

# liveness
summary(spotify1$liveness)
class(spotify1$liveness)
summary(discretize(spotify1$liveness,breaks=5))
x <- spotify1$liveness
summary( cut(x,c(-1,0.1,0.2,1)))
x <-cut(x,c(-1,0.1,0.2,1))
levels(x) <- c("0 to 0.1","0.1 to 0.2","0.2 to 1")
spotify1$liveness <- x
plot(popularity ~ liveness, data=spotify1)

# valence
summary(spotify1$valence)
class(spotify1$valence)
summary(discretize(spotify1$valence,breaks=5))
x <- spotify1$valence
summary( cut(x,c(-1,0.25,0.5,0.75,1)))
x <-cut(x,c(-1,0.25,0.5,0.75,1))
levels(x) <- c("0 to 0.25","0.25 to 0.5","0.5 to 0.75","0.75 to 1")
spotify1$valence <- x
plot(popularity ~ valence, data=spotify1)

# tempo
summary(spotify1$tempo)
class(spotify1$tempo)
summary(discretize(spotify1$tempo,breaks=5))
x <- spotify1$tempo
summary( cut(x,c(-1,100,115,130,150,230)))
x <-cut(x,c(-1,100,115,130,150,230))
levels(x) <- c("<100","100 to 115","115 to 130","130 to 150",">150")
spotify1$tempo <- x
plot(popularity ~ tempo, data=spotify1)

# time_signature
summary(spotify1$time_signature)
class(spotify1$time_signature)
summary(discretize(spotify1$time_signature,breaks=5))
x <- spotify1$time_signature
summary( cut(x,c(-1,3,4,5)))
x <-cut(x,c(-1,3,4,5))
levels(x) <- c("<4","4","5")
spotify1$time_signature <- x
plot(popularity ~ time_signature, data=spotify1)

# followers
summary(spotify1$followers)
class(spotify1$followers)
summary(discretize(spotify1$followers,breaks=5))
x <- spotify1$followers
summary( cut(x,c(-1,15000,100000,350000,2750000,100000000)))
x <-cut(x,c(-1,15000,100000,350000,2750000,100000000))
levels(x) <- c("<15,000","15,000 to 100,000","100,000 to 350,000",
               "350,000 to 2,750,000",">2,750,000")
spotify1$followers <- x
plot(popularity ~ followers, data=spotify1)

# artist_popularity
summary(spotify1$artist_popularity)
class(spotify1$artist_popularity)
summary(discretize(spotify1$artist_popularity,breaks=5))
x <- spotify1$artist_popularity
summary( cut(x,c(-1,50,60,70,80,100)))
x <-cut(x,c(-1,50,60,70,80,100))
levels(x) <- c("<50","50 to 60","60 to 70",
               "70 to 80",">80")
spotify1$artist_popularity <- x
plot(popularity ~ artist_popularity, data=spotify1)

# year
summary(spotify1$year)
class(spotify1$year)
spotify1$year = as.factor(spotify1$year)
plot(popularity ~ year, data=spotify1)

## DECISION TREE
#Combos
TREE <- rpart(popularity ~ .,data=spotify1,cp= 0.000025)
TREE$cptable
summarize_tree(TREE)
visualize_model(TREE)
# artist popularity and followers tend to play the biggest role in determining
# popularity in the decision tree



## ASSOCIATION RULES

# Overall average # of songs that have high popularity (>65 score)
mean(spotify1$popularity=="High")

SPOTIFY.TRANS <- as(spotify1,"transactions")
itemFrequency(SPOTIFY.TRANS)["popularity=High"]
#Consider pockets of size 200 or more
RULES <- apriori(SPOTIFY.TRANS,parameter = list(supp=100/length(SPOTIFY.TRANS),conf=0.045,maxlen=4),
                 appearance = list(default="lhs",rhs="popularity=High"), control=list(verbose=FALSE))
RULES <- RULES[!is.redundant(RULES)]
RULES <- RULES[is.significant(RULES,SPOTIFY.TRANS)]
length(RULES)
inspect(sort(RULES,by="confidence"))
# similar results to decision tree; some other factors do show up in the
# association rules though, such as explicit, tempo, and liveness


# summary info
summary_df_1970 <- spotify_1970s %>% 
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










