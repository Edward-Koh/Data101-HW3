
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

#define high tempo songs (above median tempo)
median_tempo <- median(spotify$tempo, na.rm = TRUE)
fast_songs <- spotify$energy[spotify$tempo > median_tempo]

#population mean energy
pop_mean_energy <- mean(spotify$energy, na.rm = TRUE)
pop_sd_energy   <- sd(spotify$energy, na.rm = TRUE)

#sample statistics for fast songs
xbar <- mean(fast_songs, na.rm = TRUE)
n <- length(fast_songs)

#z-test
z_value <- (xbar - pop_mean_energy) / (pop_sd_energy / sqrt(n))

#one-sided p-value (testing if fast songs have higher energy)
p_value <- 1 - pnorm(z_value)

z_value
p_value

        #------ section 6-------

#freq table of genre and explicity
genre_explicit_table <- table(spotify$track_genre, spotify$explicit)
genre_explicit_table

#chi test
chi_result <- chisq.test(genre_explicit_table)
chi_result

        #------ section 7 ------
library(devtools)
library(HypothesisTesting)

means_genre <- tapply(spotify$popularity, spotify$track_genre, mean)
means_genre

N <- sum(!is.na(means_genre))

# of pairwise comparison
M <- N * (N - 1) / 2

# Adjusted significance level
genre_sig <- 0.05 / M

cat("Number of genres =", N, "\n")
cat("Number of comparisons =", M, "\n")
cat("Bonferroni-adjusted alpha =", genre_sig, "\n")

pairs <- list(
  c('pop','rock'),
  c('hip-hop','jazz'),
  c('classical','electronic'),
  c('country','r-n-b'),
  c('metal','reggae')
)

for(pair in pairs){
  # permutation_test returns numeric p-value directly
  pval <- permutation_test(spotify, 'track_genre', 'popularity', 1000, pair[1], pair[2])
  
  cat(pair[1], "vs", pair[2], "→ p-value:", pval, " ")
  
  if(pval < genre_sig){
    cat(": Significant\n")
  } else {
    cat(": Not significant\n")
  }
}

              #------ Section 8--------

spotify$high_popularity <- spotify$popularity > 75
spotify$high_danceability <- spotify$danceability > 0.75

prior_prob <- sum(spotify$high_popularity) / nrow(spotify)
prior_odds <- prior_prob / (1 - prior_prob)

#Likelihood ratio
P_E_given_Popular <- sum(spotify$high_danceability & spotify$high_popularity) / sum(spotify$high_popularity)
P_E_given_NotPopular <- sum(spotify$high_danceability & !spotify$high_popularity) / sum(!spotify$high_popularity)
likelihood_ratio <- P_E_given_Popular / P_E_given_NotPopular

#Posterior odds
posterior_odds <- prior_odds * likelihood_ratio

#Posterior probability (optional)
posterior_prob <- posterior_odds / (1 + posterior_odds)

#print
cat("Prior odds =", round(prior_odds,3), 
    " Likelihood ratio =", round(likelihood_ratio,3),
    " Posterior odds =", round(posterior_odds,3),
    " Posterior probability =", round(posterior_prob,3), "\n")
