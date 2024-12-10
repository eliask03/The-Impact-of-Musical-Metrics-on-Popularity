
spotify <- read.csv("/Users/elias/Desktop/school things/Stat 306 project/spotify.csv")
head(spotify)

columns_to_remove <- c(
  "track_name", "artist.s._name", "artist_count",
  "released_year", "released_month", "released_day",
  "in_spotify_playlists", "in_spotify_charts","in_apple_playlists", 
  "in_apple_charts","in_deezer_playlists", "in_deezer_charts", 
  "in_shazam_charts", "cover_url", "key"
)

spotify <- spotify[ , !(names(spotify) %in% columns_to_remove)]
head(spotify)

# Convert streams to numeric rather than string
spotify$streams <- as.numeric(spotify$streams)

spotify$instrumentalness_. <- as.logical(spotify$instrumentalness_. > 0.5)

#remove NA forced by coercion
spotify <- spotify[!is.na(as.numeric(spotify$streams)), ]
nrow(spotify)

#test model to ensure that things are working properly
test_model = lm(streams~ bpm, data = spotify)
summary(test_model)

# Backwards Selection
models = list()

models[[1]] = lm(streams~ bpm+mode+danceability_.+valence_.+energy_.+
                   acousticness_.+instrumentalness_.+liveness_.+speechiness_.
                 , data = spotify)
summary(models[[1]])

# Fit the next model in the backward selection procedure
models[[2]] <- update(models[[1]], . ~ . - valence_.)  

models[[3]] <- update(models[[2]], . ~ . - bpm)

models[[4]] <- update(models[[3]], . ~ . - danceability_.)

models[[5]] <- update(models[[4]], . ~ . - acousticness_.)

models[[6]] <- update(models[[5]], . ~ . - energy_.)

models[[7]] <- update(models[[6]], . ~ . - mode)

models[[8]] <- update(models[[7]], . ~ . - liveness_.)

models[[9]] <- update(models[[8]], . ~ . - instrumentalness_.)


# Print the final model
summary(models[[9]])

n = nrow(spotify)
full_model = models[[1]]
sigma2_full_model = summary(full_model)$sigma^2
cp_values = numeric(length(models))
p_values= numeric(length(models))

for (i in seq_along(models)) {
  RSS_p = sum(resid(models[[i]])^2)       
  p = length(coef(models[[i]]))           
  cp_values[i] = (RSS_p / sigma2_full_model) - (n - 2 * p)
  p_values[i] = p-1 #remove one for the intercept
}

# Create the Cp plot
plot(p_values, cp_values,
     xlab = "Number of Covariates",
     ylab = "Mallows' Cp",
     main = "Cp vs. P",
     xlim = c(0, max(p_values) + 1),  
     ylim = c(0, max(cp_values) + 1))    
grid()
abline(a = 1, b = 1)

#given a plot of CP against P, the best model seems to be models[[7]]
#this three covariate model balances having a decent Cp and simple model
#most of the models have very low R^2 and the only covariate that has a 
#significant relationship with streams is speechiness

library(leaps)
s = regsubsets(streams~., data=spotify, method="forward")
ss = summary(s)
ss


df <- data.frame(
  rsq = ss$rsq,
  adjr2 = ss$adjr2,
  cp = ss$cp
)

df

forward_select_best_model = lm(streams~ danceability_. +acousticness_. 
                               + instrumentalness_.+liveness_.+speechiness_.,
                               data = spotify)

summary(forward_select_best_model)

#this model has 4 significant variables, I like the results a lot from this 
#model as it has a lot to interpret. I understand that the R^2 values are quite
#low across the board, but I think that could be something we could discuss in
#the conclusion as I have some ideas as to why that could be with regards to 
#our data set


#part 2 new analysis

# Split the data at 1 billion streams
spotify$ultra_popular <- ifelse(spotify$streams >= 1e9, 1, 0)

table(spotify$popularity_group_1B)
head(spotify)

logistic_model <- glm(ultra_popular ~ bpm + mode + danceability_. + valence_. + energy_. + 
                        acousticness_. + instrumentalness_. + liveness_. + speechiness_., 
                      data = spotify, family = binomial)

summary(logistic_model)

# Backward selection using AIC
final_logistic_model <- step(logistic_model, direction = "backward")

summary(final_logistic_model)






