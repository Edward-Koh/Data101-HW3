#load the spotify dataset
spotify <- read.csv("dataset.csv")

head(spotify)

#new variabble
spotify$EnergyDanceIndex <- spotify$energy * spotify$danceability

summary(spotify$popularity)
summary(spotify$danceability)
summary(spotify$EnergyDanceIndex)

#table examples of categorical data
table(spotify$explicit)
table(spotify$track_genre)[1:10]

#---subset examples---
#filters explicit songs
explicit_songs <- subset(spotify, explicit == 1)
explicit_songs

#filters pop songs
pop_songs <- subset(spotify, track_genre == "pop")
pop_songs

#---tapply example----
#average popularity by genre
tapply(spotify$popularity, spotify$track_genre, mean, na.rm = TRUE)[1:10]

#average energy by explicit/non-explicit
tapply(spotify$energy, spotify$explicit, mean)
