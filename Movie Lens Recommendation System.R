#############################################################
# Create edx set, validation set, and submission file
#############################################################

# Note: this process could take a couple of minutes

if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(caret)) install.packages("caret", repos = "http://cran.us.r-project.org")

# MovieLens 10M dataset:
# https://grouplens.org/datasets/movielens/10m/
# http://files.grouplens.org/datasets/movielens/ml-10m.zip

dl <- tempfile()
download.file("http://files.grouplens.org/datasets/movielens/ml-10m.zip", dl)

ratings <- read.table(text = gsub("::", "\t", readLines(unzip(dl, "ml-10M100K/ratings.dat"))),
                      col.names = c("userId", "movieId", "rating", "timestamp"))

movies <- str_split_fixed(readLines(unzip(dl, "ml-10M100K/movies.dat")), "\\::", 3)
colnames(movies) <- c("movieId", "title", "genres")
movies <- as.data.frame(movies) %>% mutate(movieId = as.numeric(levels(movieId))[movieId],
                                           title = as.character(title),
                                           genres = as.character(genres))

movielens <- left_join(ratings, movies, by = "movieId")

# Validation set will be 10% of MovieLens data

set.seed(1, sample.kind="Rounding") #set.seed(1)
test_index <- createDataPartition(y = movielens$rating, times = 1, p = 0.1, list = FALSE)
edx <- movielens[-test_index,]
temp <- movielens[test_index,]

# Make sure userId and movieId in validation set are also in edx set

validation <- temp %>% 
  semi_join(edx, by = "movieId") %>%
  semi_join(edx, by = "userId")

# Add rows removed from validation set back into edx set

removed <- anti_join(temp, validation)
edx <- rbind(edx, removed)

rm(dl, ratings, movies, test_index, temp, movielens, removed)

# required packages for our project
if(!require(kableExtra)) install.packages('kableExtra', 
                                          repos = 'http://cran.us.r-project.org')
if(!require(dataCompareR)) install.packages('dataCompareR', 
                                            repos = 'http://cran.us.r-project.org')
if(!require(tidyverse)) install.packages('tidyverse', 
                                         repos = 'http://cran.us.r-project.org')
if(!require(caret)) install.packages('caret', 
                                     repos = 'http://cran.us.r-project.org')
if(!require(data.table)) install.packages('data.table', 
                                          repos = 'http://cran.us.r-project.org')

# Loading all needed libraries

library(kableExtra)
library(dataCompareR)
library(tidyverse)
library(caret)
library(data.table)
library(stringr)
library(ggplot2)



# Save our data as R objects
save(edx, file = 'edx.RData')
save(validation, file = 'validation.RData')

# The data is then accessed using the load function
load('edx.RData')
load('validation.RData')

as_tibble(edx) %>%
  slice(1:5) %>%
  knitr::kable()

as_tibble(validation) %>%
  slice(1:5) %>%
  knitr::kable()

library(dataCompareR)
comp_edx_val <- rCompare(edx, validation)
comp_summ <- summary(comp_edx_val)
comp_summ[c('datasetSummary', 'ncolInAOnly', 'ncolInBOnly', 'ncolCommon', 'rowsInAOnly', 'rowsInBOnly', 'nrowCommon')] 


##Duplication Checks

# Distinct users, movies, genres
dist_col <- edx %>% 
  summarize(distinct_users = n_distinct(userId),
            distinct_movies = n_distinct(movieId),
            distinct_genres = n_distinct(genres))
knitr::kable(dist_col)


#transform tables 


tidydf <- function(df){
  df$genres <- as.factor(df$genres) #Convert genres to factor
  df$timestamp <- as.Date(as.POSIXct(df$timestamp, origin='1970-01-01'))
  #Convert timestamp
  names(df)[names(df) == 'timestamp'] <- 'rate_year' # Rename column timestamp to rate_year
  df <- df %>% 
    mutate(title = str_trim(title), rate_year = year(rate_year)) %>%  #Mutate title and rate_year
    extract(title, c('title', 'premier_year'), regex = '(.*)\\s\\((\\d+)\\)', convert = TRUE) 
  #Separate title from year
  return(df)
}
# Transform our dataframes
edx <- tidydf(edx)
validation <- tidydf(validation)

as_tibble(edx)
as_tibble(validation)


##Missing Value Analysis

# Check edx dataframe for missing values
edx_na <- edx %>%
  filter(is.na(title) | is.na(year))
glimpse(edx_na) 
# Check validation dataframe for missing values
validation_na <- validation %>%
  filter(is.na(title) | is.na(year))
glimpse(validation_na) 



###Ratings Destribution


# Check frequencies of ratings unique values
table_rating <- as.data.frame(table(edx$rating))
colnames(table_rating) <- c('Ratings', 'Frequency')
knitr::kable(table_rating)


###Graph plot of the Ratings Destribution:


# Frequency plot of the ratings
table_rating %>% ggplot(aes(Ratings, Frequency)) +
  geom_bar(stat = 'identity') +
  labs(x='Ratings', y='Frequency') +
  ggtitle('Ratings Frequency Destribution')


### Top 20 Movies (by views) 

# Top movies by number of views
tmovies <- edx %>% select(title) %>% 
  group_by(title) %>% 
  summarize(count=n()) %>% 
  arrange(desc(count)) 
# Print top_movies
knitr::kable(head(tmovies,20))


### Average Movie Ratings (Top 20 by Rating AVG)


# Top movies by rating average
rating_avg <- edx %>%
  select(title, rating) %>%
  group_by(title) %>%
  summarise(count = n(), avg = mean(rating), min = min(rating), max = max(rating)) %>%
  arrange(desc(avg))
# Print top_movies
knitr::kable(head(rating_avg,20))

# Top movies by rating average
rating_avg_200 <- edx %>%
  select(title, rating) %>%
  group_by(title) %>%
  summarise(count = n(), avg = mean(rating), min = min(rating), max = max(rating)) %>%
  filter(count > 200) %>%  
  arrange(desc(avg))
# Print top_movies
knitr::kable(head(rating_avg_200,20))

rating_avg_200 %>% 
  ggplot(aes(x= avg, fill = count)) +
  geom_histogram( binwidth = 0.2) +
  scale_x_continuous(breaks=seq(0, 5, by= 0.5)) +
  labs(x='Average Ratings', y='Frequency') +
  ggtitle('Destribution of Average Movie Ratings') 


### Data Heat Map


# We create a copy of existing edx
edx_copy <-edx
# Sample of 100 users 
users <- sample(unique(edx_copy$userId), 1000)
edx_copy %>% filter(userId %in% users) %>% 
  select(userId, movieId, rating) %>%
  mutate(rating = 1) %>%
  spread(movieId, rating) %>% select(sample(ncol(.), 1000)) %>% 
  as.matrix() %>% t(.) %>%
  image(1:1000, 1:1000,., col = 'blue', xlab='Movies', ylab='Users', main = 'Heatmap of the movie rates matrix')



###Movie to User Destribution

edx %>%  count(movieId) %>% 
  ggplot(aes(n)) + 
  geom_histogram(bins = 30, color = 'black') + 
  scale_x_log10() + 
  ggtitle('Distribution of movies')


###Movie to User Activity


edx %>%  count(userId) %>% 
  ggplot(aes(n)) + 
  geom_histogram(bins = 30, color = 'black') + 
  scale_x_log10() + 
  ggtitle('Distribution of users')



##Destribution by Genre

# Top movies by number of views
tgen <- edx %>% select(genres) %>% 
  group_by(genres) %>% 
  summarize(count=n()) %>% 
  arrange(desc(count)) 
# Print top_movies
knitr::kable(head(tgen,20))


## Numbers of Ratings per Movie

ggplot(edx, aes(movieId)) +
  theme_classic()  +
  geom_histogram(binwidth=500) +
  labs(title = "Ratings Frequency Distribution Per Title",
       x = "Title (ID for Movie)",
       y = "Frequency")

## Top Rated Movies


edx %>%
  group_by(title) %>%
  summarise(count = n()) %>%
  arrange(desc(count)) %>%
  head(n=25) %>%
  ggplot(aes(title, count)) +
  theme_classic()  +
  geom_col() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, size = 7)) +
  labs(title = "Ratings Frequency Distribution - TOP 25 Movies (Alphabetical Order)",
       x = "Title",
       y = "Frequency")



### Rating Distribution per Genre


# Extract the genre in edx

edx <- edx %>%
  mutate(genre = fct_explicit_na(genres,
                                 na_level = "(no genres listed)")
  ) %>%
  separate_rows(genre,
                sep = "\\|")

# Extract the genre in validation 

validation <- validation %>%
  mutate(genre = fct_explicit_na(genres,
                                 na_level = "(no genres listed)")
  ) %>%
  separate_rows(genre,
                sep = "\\|")

edx %>%
  group_by(genre) %>%
  summarise(count = n()) %>%
  ggplot(aes(genre, count)) +
  theme_classic()  +
  geom_col() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  labs(title = "Ratings Frequency Distribution Per Genre",
       x = "Genre",
       y = "Frequency")

edx %>%
  group_by(genre) %>%
  summarise(count = n()) %>%
  arrange(desc(count)) %>%
  kable() %>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed", "responsive"),
                position = "center",
                font_size = 10,
                full_width = FALSE)

### Mean Distribution per Genre

edx %>%
  group_by(genre) %>%
  summarise(mean = mean(rating)) %>%
  ggplot(aes(genre, mean)) +
  theme_classic()  +
  geom_col() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  labs(title = "Mean Distribution per Genre",
       x = "Genre",
       y = "Mean")

edx %>%
  group_by(genre) %>%
  summarise(mean = mean(rating)) %>%
  arrange(desc(mean)) %>%
  head(n=35) %>%
  kable() %>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed", "responsive"),
                position = "center",
                font_size = 10,
                full_width = FALSE)



##Model Building

### Model 1 : Computing predicted ratings for all movies regardless of user (Naive)

# Ratings for all movies
mu_hat <- mean(edx$rating)
mu_hat


#RMSE function
RMSE <- function(true_ratings, predicted_ratings){
  sqrt(mean((true_ratings - predicted_ratings)^2))
}

# RMSE calculation
simple_model_rmse <- RMSE(validation$rating, mu_hat) 
simple_model_rmse

rmse_values <- tibble(method = 'Simple model RMSE', RMSE = simple_model_rmse)
knitr::kable(rmse_values)



### Model 2 : Computing predicted ratings for all movies based on movie effects


#Compute the average of all ratings of the edx set
mu <- mean(edx$rating)
#Compute b_i
movie_avgs <- edx %>% 
  group_by(movieId) %>% 
  summarize(b_i = mean(rating - mu))

#Plot b_i distribution
movie_avgs %>% 
  ggplot(aes(b_i)) + 
  geom_histogram(bins = 30, color = 'black') + 
  ggtitle('Distribution of estimated b_i')

# Predict bi
model_2_pred <- mu + validation %>% 
  left_join(movie_avgs, by='movieId') %>%
  .$b_i
movie_effect_rmse <- RMSE(validation$rating, model_2_pred)
# Enter RMSE value in table 
rmse_values <- bind_rows(rmse_values,
                         tibble(method='Movie Effect Model',  
                                RMSE = movie_effect_rmse))
knitr::kable(rmse_values)


### Model 3 : Computing predicted ratings for all movies based on movie and user effects



# Compute average rating for user u who rated more than 100 movies
edx %>% 
  group_by(userId) %>% 
  summarize(b_u = mean(rating)) %>% 
  filter(n()>=100) %>%
  ggplot(aes(b_u)) + 
  geom_histogram(bins = 30, color = 'black') + 
  ggtitle('Distribution of estimated b_u')

#Compute b_u on edx 
user_avgs <- edx %>%  
  left_join(movie_avgs, by='movieId') %>%
  group_by(userId) %>%
  summarize(b_u = mean(rating - mu - b_i))

# Predicted ratings
model3_pred <- validation %>% 
  left_join(movie_avgs, by='movieId') %>%
  left_join(user_avgs, by='userId') %>%
  mutate(pred = mu + b_i + b_u) %>%
  pull(pred)
movie_user_effect <- RMSE(validation$rating, model3_pred)
rmse_values <- bind_rows(rmse_values,
                         tibble(method='Movie + User Effects Model',  
                                RMSE = movie_user_effect))
knitr::kable(rmse_values)


### Model 4 : Computing predicted ratings for all movies based on movie and user effects and genre
 
genre_avgs <- edx %>%
  left_join(movie_avgs, by='movieId') %>%
  left_join(user_avgs, by='userId') %>%
  group_by(genre) %>%
  summarize(b_g = mean(rating - mu - b_i - b_u))


###Improving RMSE

# Ratings on validation dataset

model4_pred <- validation %>%
  left_join(movie_avgs, by='movieId') %>%
  left_join(user_avgs, by='userId') %>%
  left_join(genre_avgs, by='genre') %>%
  mutate(pred2 = mu + b_i + b_u + b_g) %>%
  pull(pred2)

# Adding the results to the results dataset
movie_user_genre_effect <- RMSE(validation$rating, model4_pred)
rmse_values <- bind_rows(rmse_values,
                         tibble(method='Movie + User + Genre Effects Model',  
                                RMSE = movie_user_genre_effect))
knitr::kable(rmse_values)



## Regularization


validation %>%
  left_join(movie_avgs, by='movieId') %>%
  mutate(residual = rating - (mu + b_i)) %>%
  arrange(desc(abs(residual))) %>%
  select(title, residual) %>% slice(1:10)




# merged database of MOvie and Title
merge_db <- edx %>% 
  select(movieId, title) %>%
  distinct()


###Top 10 Best Movies 


# top 10 best movies based on b_i
movie_avgs %>% left_join(merge_db, by="movieId") %>%
  arrange(desc(b_i)) %>%
  select(title, b_i) %>%
  slice(1:10) 
validation %>% count(movieId) %>%
  left_join(movie_avgs) %>%
  left_join(merge_db, by="movieId") %>%
  arrange(desc(b_i)) %>%
  select(title, b_i, n) %>%
  slice(1:10)



###Top 10 Worst Movies 


# top 10 worse movies based on b_i
movie_avgs %>% left_join(merge_db, by="movieId") %>%
  arrange(b_i) %>%
  select(title, b_i) %>%
  slice(1:10)
#knitr::kable(movie_avgs)
validation %>% count(movieId) %>%
  left_join(movie_avgs) %>%
  left_join(merge_db, by="movieId") %>%
  arrange(b_i) %>%
  select(title, b_i, n) %>%
  slice(1:10)


### Penalized least squares


lambdas <- seq(0, 10, 0.25)
mu <- mean(edx$rating)
just_the_sum <- edx %>% 
  group_by(movieId) %>% 
  summarize(s = sum(rating - mu), n_i = n())
rmses <- sapply(lambdas, function(l){
  predicted_ratings <- validation %>% 
    left_join(just_the_sum, by='movieId') %>% 
    mutate(b_i = s/(n_i+l)) %>%
    mutate(pred = mu + b_i) %>%
    pull(pred)
  return(RMSE(predicted_ratings, validation$rating))
})


###Plotting RMSE values together with lambdas


# Plot lambdas and rmse
ggplot(data.frame(lambdas = lambdas, rmses = rmses ), aes(lambdas, rmses)) +
  ggtitle('RMSEs vs Lambdas (Movie + User Model)')  +
  geom_point()
lambdas[which.min(rmses)]




lambdas <- seq(0, 10, 0.25)
rmses <- sapply(lambdas, function(l){
  mu <- mean(edx$rating)
  b_i <- edx %>% 
    group_by(movieId) %>%
    summarize(b_i = sum(rating - mu)/(n()+l))
  
  b_u <- edx %>% 
    left_join(b_i, by='movieId') %>%
    group_by(userId) %>%
    summarize(b_u = sum(rating - b_i - mu)/(n()+l))
  predicted_ratings <- 
    validation %>% 
    left_join(b_i, by = 'movieId') %>%
    left_join(b_u, by = 'userId') %>%
    mutate(pred = mu + b_i + b_u) %>%
    pull(pred)
  
  return(RMSE(validation$rating, predicted_ratings))
})
ggplot(data.frame(lambdas = lambdas, rmses = rmses ), aes(lambdas, rmses)) +
  ggtitle('RMSEs vs Lambdas (Regularized Movie + User Model)') +
  geom_point()  

###Full model

# Value of lambda that minimizes  RMSE
lambda <- lambdas[which.min(rmses)]
lambda

# Add model with the minimal RMSE to the results data frame
rmse_values <- bind_rows(
  rmse_values,
  tibble(method='Regularized Movie + User Effect Model',  
         RMSE = min(rmses)))
knitr::kable(rmse_values)



#Conclusion


knitr::kable(rmse_values)
