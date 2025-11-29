
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

              #------ section 3 ------
set.seed(123)

n_rows <- nrow(spotify)

subset_rows <- sample(1:n_rows, 10000, replace = FALSE)
spotify_subset <- spotify[subset_rows, ]

cor_original <- cor(spotify_subset$key, spotify_subset$tempo)
cor_original

n_perm <- 1000
cor_perm <- numeric(n_perm)

for (i in 1:n_perm) {
  shuffled_key <- sample(spotify_subset$key)
  cor_perm[i] <- cor(spotify_subset$tempo, shuffled_key)
}

# Two-tailed p-value
p_value <- mean(abs(cor_perm) >= abs(cor_original))
p_value

              # ------- section 4------

n_samples <- 1000
sample_size <- 50

#take random samples of popularity and compute means
sample_means <- replicate(n_samples, {
  mean(sample(spotify$popularity, sample_size, replace = TRUE))
})

#plot distribution
hist(sample_means, breaks = 30, col = "lightblue",
     main = "Distribution of Sample Means (Popularity)",
     xlab = "Sample Mean Popularity"
    )

#compute 95% confidence interval
mean_pop <- mean(sample_means)
se <- sd(sample_means)
ci_lower <- mean_pop - 1.96 * se
ci_upper <- mean_pop + 1.96 * se

ci_lower
ci_upper

              #----- section 5--------


