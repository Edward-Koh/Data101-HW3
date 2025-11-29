
                #----Section 1------
#load the spotify dataset

spotify <- read.csv("dataset.csv")

head(spotify)


#new variable
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

                  #------ Section 2-------
#Histogram of Popularity(numeric)
hist(
  spotify$popularity,
  main = "Distribution of Track Popularity",
  xlab = "Popularity",
  breaks = 30
)

#Barplot of Explicit vs Clean Tracks (categorical)
explicit_counts <- table(spotify$explicit)

barplot(
  explicit_counts,
  main = "Explicit vs Non-Explicit Tracks",
  xlab = "Explicit (0 = Clean, 1 = Explicit)",
  ylab = "Count"
)
#values of each plot
breaks <- seq(0, 100, by = 5)
table(cut(spotify$popularity, breaks, include.lowest = TRUE, right = TRUE))
table(spotify$explicit)

