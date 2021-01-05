## Loy Jong Sheng
## MovieLens Project.R 
## HarvardX PH125.9x Data Science: Capstone - MovieLens Project
## title: MovieLens
## date: 10/17/2020
##############################################################
# MovieLens Project R Script 
##############################################################

## ----setup, include=FALSE---------------------------------------------------------------------------------------------------------------
library(tinytex)
library(knitr)
knitr::opts_chunk$set(echo = FALSE)


## ----Load libraries, echo=TRUE, message=FALSE, warning=FALSE----------------------------------------------------------------------------
if(!require(tidyverse)) install.packages("tidyverse", 
                                      repos = "http://cran.us.r-project.org")
if(!require(caret)) install.packages("caret", 
                                      repos = "http://cran.us.r-project.org")
if(!require(data.table)) install.packages("data.table", 
                                      repos = "http://cran.us.r-project.org")
if(!require(ggplot2)) install.packages("ggplot2", 
                                      repos = "http://cran.us.r-project.org")
if(!require(lubridate)) install.packages("lubridate", 
                                      repos = "http://cran.us.r-project.org")
if(!require(recosystem)) install.packages("recosystem", 
                                      repos = "http://cran.us.r-project.org")
if(!require(Matrix)) install.packages("Matrix", 
                                      repos = "http://cran.us.r-project.org")
if(!require(recommenderlab)) install.packages("recommenderlab", 
                                      repos = "http://cran.us.r-project.org")
if(!require(tinytex)) install.packages("tinytex", 
                                      repos = "http://cran.us.r-project.org")
library(tidyverse)
library(caret)
library(data.table)
library(ggplot2)
library(lubridate)
library(recosystem)
library(Matrix)
library(recommenderlab)


## ----download the data files and data preparation, echo=TRUE, message=FALSE, warning=FALSE----------------------------------------------
# MovieLens 10M dataset:
# https://grouplens.org/datasets/movielens/10m/
# http://files.grouplens.org/datasets/movielens/ml-10m.zip

dl <- tempfile()
download.file("http://files.grouplens.org/datasets/movielens/ml-10m.zip", dl)

ratings <- fread(text = gsub("::", "\t", readLines(unzip(dl, "ml-10M100K/ratings.dat"))),
                 col.names = c("userId", "movieId", "rating", "timestamp"))

movies <- str_split_fixed(readLines(unzip(dl, "ml-10M100K/movies.dat")), "\\::", 3)
colnames(movies) <- c("movieId", "title", "genres")

# if using R 4.0 or later:
movies <- as.data.frame(movies) %>% mutate(movieId = as.numeric(movieId),
                                           title = as.character(title),
                                           genres = as.character(genres))

movielens <- left_join(ratings, movies, by = "movieId")


## ----Create training and test dataset, echo=TRUE, message=FALSE, warning=FALSE----------------------------------------------------------
# Validation set will be 10% of MovieLens data
set.seed(1, sample.kind="Rounding") # if using R 3.5 or earlier, use `set.seed(1)`
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
#Remove Objects from a Specified Environment
rm(dl, ratings, movies, test_index, temp, movielens, removed)


## ----Explore training dataset edx, echo=TRUE--------------------------------------------------------------------------------------------
head(edx)


## ----summarize the number of distinct parameters, echo=TRUE-----------------------------------------------------------------------------
edx %>% 
  summarize(Num_Distinct_Movie = n_distinct(movieId), 
            Num_Distinct_User = n_distinct(userId), 
            Num_Distinct_Genres = n_distinct(genres), 
            Tot_size = nrow(edx))

## ----summary of edx dataset, echo=TRUE--------------------------------------------------------------------------------------------------
summary(edx)


## ----Ratings given by users, echo=TRUE, message=FALSE, warning=FALSE--------------------------------------------------------------------
#Top 5 ratings in order from most to least
Top_Given_Ratings <- edx %>% group_by(rating) %>%
  summarize(numRatings = n()) %>% 
  arrange(desc(numRatings)) %>% 
  top_n(5, numRatings)
Top_Given_Ratings

## ----Plot Ratings given by users, echo=TRUE, message=FALSE, warning=FALSE---------------------------------------------------------------
Given_Ratings <- edx %>% group_by(rating) %>%  summarize(numRatings = n()) 
ggplot(data = Given_Ratings, mapping = aes(x = rating, y = numRatings)) + 
  geom_col(fill = "blue", color = "grey") + 
  labs(y = "Num of Ratings", x = "Ratings") + 
  ggtitle("Distribution of Ratings given by Users")

## ----summary of rating, echo=TRUE, message=FALSE, warning=FALSE-------------------------------------------------------------------------
summary(edx$rating)


## ----Plot Ratings, echo=TRUE, message=FALSE, warning=FALSE------------------------------------------------------------------------------
NumRating_per_UserID <- edx %>% group_by(userId) %>%  summarize(numRating = n()) 

NumRating_per_UserID %>% 
  ggplot(aes(x = userId, y = numRating)) + 
  geom_col(color = "blue") +
  geom_hline(yintercept= mean(NumRating_per_UserID$numRating), 
             linetype="dashed", color = "red") + 
  geom_text(aes(y=mean(NumRating_per_UserID$numRating)+500, 
                label="Mean Num of Rating", x=30000.0), 
                colour="red", angle=0) + 
  labs(x = "UserId", y = "Num of Rating") + 
  ggtitle("Distribution of Users' Number of ratings")


## ----Top 5 Active users, echo=TRUE, message=FALSE, warning=FALSE------------------------------------------------------------------------
#Top 5 active users
Top_Active_Users <- edx %>% group_by(userId) %>% 
  summarize(numRating = n()) %>%
  arrange(desc(numRating)) %>%
  top_n(5, numRating)
Top_Active_Users

## ----Bottom 3 Active users, echo=TRUE, message=FALSE, warning=FALSE---------------------------------------------------------------------
#Bottom 3 active users
Top_Active_Users <- edx %>% group_by(userId) %>% 
  summarize(numRating = n()) %>%
  arrange(desc(numRating)) %>%
  top_n(-3, numRating)
Top_Active_Users


## ----Plot Yearly Ratings from 1995 to 2009, echo=TRUE, message=FALSE, warning=FALSE-----------------------------------------------------
# Yearly Rating from 1995 to 2009
Yearly_Ratings <- edx %>% 
  group_by(year = format(as_datetime(timestamp), format("%Y"))) %>%  
  summarize(numRatings = n())
ggplot(data = Yearly_Ratings, mapping = aes(x = year, y = numRatings)) + 
  geom_col(fill = "blue", color = "grey") + 
  labs(x = "Year", y = "Num of Ratings") + 
  ggtitle("Distribution of Yearly Ratings Given ")


## ----Plot Yearly Number of Users, echo=TRUE, message=FALSE, warning=FALSE---------------------------------------------------------------
# Yearly Number of Users from 1995 to 2009
Yearly_NumUsers <- edx %>% 
  group_by(year = format(as_datetime(timestamp), format("%Y"))) %>%  
  summarize(numUsers = n_distinct(userId))
Yearly_NumUsers %>% ggplot(aes(x = year, y = numUsers)) + 
  geom_col(fill = "blue", color = "grey") + 
  labs(x = "Year", y = "Num of Users") + 
  ggtitle("Distribution of Yearly Number of Users")


## ----Plot Yearly Number of Average number of rating per User, echo=TRUE, message=FALSE, warning=FALSE-----------------------------------
# Yearly Average number of Rating Per User from 1995 to 2009
Yearly_AvgNumRatingPerUser <- edx %>% 
  group_by(year = format(as_datetime(timestamp), format("%Y"))) %>%  
  summarize(AvgNumRatingPerUser = n()/n_distinct(userId))
Yearly_AvgNumRatingPerUser %>% 
  ggplot(aes(x = year, y = AvgNumRatingPerUser)) + 
  geom_col(fill = "blue", color = "grey") + 
  labs(x = "Year", y = "Avg Num of Ratings Per User") + 
  ggtitle("Distribution of Yearly Average number of Rating Per User")


## ----Plot Yearly Number of Average number of rating per Movie, echo=TRUE, message=FALSE, warning=FALSE----------------------------------
# Yearly Yearly Number of Average number of rating per Movie from 1995 to 2009
Yearly_AvgRatingPerUser <- edx %>% 
  group_by(year = format(as_datetime(timestamp), format("%Y"))) %>%  
  summarize(AvgRatingPerUser = sum(rating)/n())
Yearly_AvgRatingPerUser %>% ggplot(aes(x = year, y = AvgRatingPerUser)) + 
  geom_col(fill = "blue", color = "grey") + 
  geom_hline(yintercept= mean(edx$rating), linetype="dashed", color = "red") + 
  geom_text(aes(y=mean(edx$rating), label="Mean Rating", x=3.0), colour="red", angle=0) + 
  labs(x = "Year", y = "Average Ratings") + 
  ggtitle("Distribution of Yearly Average Rating")


## ----Top Movies, echo=TRUE, message=FALSE, warning=FALSE--------------------------------------------------------------------------------
#Top years with most number of ratings
Top_Movies <- edx %>% group_by(movieId) %>%
  summarize(numRatings = n(), movieTitle = first(title)) %>%
  arrange(desc(numRatings)) %>%
  top_n(10, numRatings)
Top_Movies


## ----Plot number of rating per movieId, echo=TRUE, message=FALSE, warning=FALSE---------------------------------------------------------
edx %>% group_by(movieId) %>% summarize(n = n()) %>% 
  ggplot(aes(n)) + 
  geom_histogram(fill = "blue", color = "grey", bins = 30) + 
  scale_x_log10() + ggtitle("Distribution of MovieId") + 
  labs(x="MovieId" , y="Number of ratings")

## ----Plot Avg Rating per MovieId, echo=TRUE, message=FALSE, warning=FALSE---------------------------------------------------------------
AvgRating_per_MovieId <- edx %>% group_by(movieId) %>% 
  summarize( AvgRatingsPerMovieId = sum(rating)/n()) 
AvgRating_per_MovieId %>% ggplot(aes(x = movieId, y = AvgRatingsPerMovieId)) + 
  geom_hline(yintercept= mean(edx$rating), linetype="dashed", color = "red") + 
  geom_text(aes(y=mean(edx$rating), label="Mean Rating", x=20000.0), colour="red") + 
  geom_col(color = "blue") + labs(x = "MovieId", y = "Average Rating") + 
  ggtitle("Distribution of Avg Ratings per MovieId")


## ----Top Genres, echo=TRUE, message=FALSE, warning=FALSE--------------------------------------------------------------------------------
#Top genres
Top_genres_rating <- edx %>% group_by(genres) %>%
  summarize(numRatings = n()) %>%
  arrange(desc(numRatings)) %>%
  top_n(10, numRatings)
Top_genres_rating

## ----Split the genres, echo=TRUE--------------------------------------------------------------------------------------------------------
#Split the genres
Genres_split <- edx %>% separate_rows(genres, sep = "\\|")
Genres_split %>% summarize(Total_Distinct_Genre = n_distinct(genres))

## ----Plot genres vs number of ratings per genre, echo=TRUE, message=FALSE, warning=FALSE------------------------------------------------
Distinct_genres_group <- Genres_split %>% 
  group_by(genres) %>% 
  summarize(numRatings = n())
Distinct_genres_group %>%
  ggplot(aes(reorder(genres, numRatings), numRatings, fill= numRatings)) +
  geom_bar(stat = "identity") + coord_flip() +
  scale_fill_distiller(palette = "Blues") + labs(y = "Num of Ratings Per Genre ", x = "Genres") +
  ggtitle("Distribution of Number of Ratings per Genre")

## ----get movie with no genres listed, echo=TRUE, message=FALSE, warning=FALSE-----------------------------------------------------------
NoGenreListed <- Genres_split %>% 
  filter(genres == "(no genres listed)") %>% 
  summarize(userid = userId, rating = rating, movieid = movieId, 
            movieTitle = title, genres = genres)
NoGenreListed


## ----Top 10 genres each year, echo=TRUE, message=FALSE, warning=FALSE-------------------------------------------------------------------
YearGenresGrp <- Genres_split %>% 
  group_by(genres, year = format(as_datetime(timestamp), format("%Y"))) %>% 
  summarize(numRatings = n())
Top10GenreYearly <- YearGenresGrp %>% group_by(year) %>% 
  arrange(desc(numRatings)) %>% 
  top_n(10, numRatings) %>% 
  mutate(ranks = rank(numRatings))
Top10GenreYearly %>% ggplot(aes(x = year, y = ranks, color = genres)) + 
  geom_point(size=3) + 
  xlab("Year") + ylab("Ranks") + 
  ggtitle("Distribution of Yearly Top Genres")


## ----Plot genres vs number of movie per genre, echo=TRUE, message=FALSE, warning=FALSE--------------------------------------------------
#Split the genres
Movie_per_Genres_group <- Genres_split %>% 
  group_by(genres) %>% 
  summarize( numMovies = n_distinct(movieId))
Movie_per_Genres_group %>%
  ggplot(aes(reorder(genres, numMovies), numMovies, fill= numMovies)) +
  geom_bar(stat = "identity") + coord_flip() +
  scale_fill_distiller(palette = "Blues") + 
  labs(y = "Num of Movies Per Genre", x = "Genres") +
  ggtitle("Distribution of Number of Movies per Genre")


## ----Plot genres vs number of Ratings, echo=TRUE, message=FALSE, warning=FALSE----------------------------------------------------------
AvgNumRating_per_Movie_group <- Genres_split %>% 
  group_by(genres) %>% 
  summarize( AvgnumRatingsPerGenre = n()/n_distinct(movieId))
AvgNumRating_per_Movie_group %>%
  ggplot(aes(reorder(genres, AvgnumRatingsPerGenre), 
             AvgnumRatingsPerGenre, fill= AvgnumRatingsPerGenre)) +
  geom_bar(stat = "identity") + coord_flip() +
  scale_fill_distiller(palette = "Blues") + 
  labs(y = "Avg Num of Ratings", x = "Genres") +
  ggtitle("Distribution of Average Number of Ratings per Genre")


## ----Plot Avg Rating per Genre, echo=TRUE, message=FALSE, warning=FALSE-----------------------------------------------------------------
AvgRating_per_Genre_group <- Genres_split %>% group_by(genres) %>% 
  summarize( AvgRatingsPerGenre = sum(rating)/n())
AvgRating_per_Genre_group %>%
  ggplot(aes(reorder(genres, AvgRatingsPerGenre), AvgRatingsPerGenre, fill= AvgRatingsPerGenre)) +
  geom_bar(stat = "identity") + coord_flip() + 
  geom_hline(yintercept= mean(Genres_split$rating), linetype="dashed", color = "red") +
  geom_text(aes(y=mean(Genres_split$rating), label="Mean Rating", x=3.0), colour="red", angle=90) +
  scale_fill_distiller(palette = "Blues") + labs(y = "Avg Ratings Given", x = "Genres") +
  ggtitle("Distribution of Average Ratings per Genre")


## ----RMSE Evaluation, echo=TRUE, message=FALSE, warning=FALSE---------------------------------------------------------------------------
#Define the Loss function
RMSE <- function(true_ratings, predicted_ratings){
    sqrt(mean((true_ratings - predicted_ratings)^2))
}
#Create a RMSE results dataframe to store and compare the results.
RMSE_results <- tibble()


## ----Baseline model, echo=TRUE, message=FALSE, warning=FALSE----------------------------------------------------------------------------
#Assumes the same rating for all movies and users with all the differences explained by random variation. 
#Predicting using mu_hat
mu_hat <- mean(edx$rating)


## ----RMSE Baseline model, echo=TRUE, message=FALSE, warning=FALSE-----------------------------------------------------------------------
rmse <- RMSE(validation$rating, mu_hat)
#Add result to the results dataframe
RMSE_results <- tibble(Model = "Mean_hat", 
                       Desciption = "baseline (using mean hat only)", 
                       RMSE_value = rmse)
RMSE_results


## ----Modeling movie effect, echo=TRUE, message=FALSE, warning=FALSE---------------------------------------------------------------------
#Compute the movie effect
Movie_effects <- edx %>% group_by(movieId) %>% 
  summarize(b_i = mean(rating - mu_hat))
#Examine the movie effects
Movie_effects %>% ggplot(aes(x = b_i)) + 
  geom_histogram(bins = 30, fill = "blue", color = "grey") + 
  ggtitle("Distribution of Movie effect")


## ----Predicting with movie effect, echo=TRUE, message=FALSE, warning=FALSE--------------------------------------------------------------
#Predicting Baseline + movie effect
Predicted_ratings <- mu_hat + validation %>% 
  left_join(Movie_effects, by='movieId') %>%  pull(b_i)


## ----RMSE Baseline with  movie effect, echo=TRUE, message=FALSE, warning=FALSE----------------------------------------------------------
rmse <- RMSE(Predicted_ratings, validation$rating)
RMSE_results <- RMSE_results %>% 
  add_row(Model = "baseline+b_i", 
          Desciption = "baseline with movie effect", 
          RMSE_value = rmse)
RMSE_results


## ----Modelling with movie and user effects, echo=TRUE, message=FALSE, warning=FALSE-----------------------------------------------------
#Compute the user effects
User_effects <- edx %>% left_join(Movie_effects, by='movieId') %>%  
  group_by(userId) %>% summarize(b_u = mean(rating - mu_hat - b_i))
#Examine the user effects
User_effects %>% ggplot(aes(x = b_u)) + 
  geom_histogram(bins = 30, fill = "blue", color = "grey") + 
  ggtitle("Distribution of User effect")


## ----Predicting with movie and user effects, echo=TRUE, message=FALSE, warning=FALSE----------------------------------------------------
#Predicting Baseline + movie effects + user effects
Predicted_ratings <- validation %>% left_join(Movie_effects, by='movieId') %>% 
  left_join(User_effects, by='userId') %>%
  mutate(pred = mu_hat + b_i + b_u) %>% pull(pred)


## ----RMSE Baseline with  movie and user effects, echo=TRUE, message=FALSE, warning=FALSE------------------------------------------------
rmse <- RMSE(Predicted_ratings, validation$rating)
RMSE_results <- RMSE_results %>% 
  add_row(Model = "baseline+b_i+b_u", 
          Desciption = "baseline with movie+user effects", 
          RMSE_value = rmse)
RMSE_results


## ----Modelling with movie, user and genre effects, echo=TRUE, message=FALSE, warning=FALSE----------------------------------------------
#Compute the genre effects
Genre_effects <- edx %>% left_join(Movie_effects, by='movieId') %>% 
  left_join(User_effects, by='userId') %>%  
  group_by(genres) %>% summarize(b_g = mean(rating - mu_hat - b_i - b_u))
#Examine the genre effects
Genre_effects %>% ggplot(aes(x = b_g)) + 
  geom_histogram(bins = 30, fill = "blue", color = "grey") + 
  ggtitle("Distribution of Genre effects")


## ----Predicting with movie, user and genre effects, echo=TRUE, message=FALSE, warning=FALSE---------------------------------------------
#Predicting Baseline + movie effect + user effect + genre effects
Predicted_ratings <- validation %>% left_join(Movie_effects, by='movieId') %>% 
  left_join(User_effects, by='userId') %>% left_join(Genre_effects, by='genres') %>%
  mutate(pred = mu_hat + b_i + b_u + b_g) %>% pull(pred)



## ----RMSE Baseline with  movie, user and genre effects, echo=TRUE, message=FALSE, warning=FALSE-----------------------------------------
rmse <- RMSE(Predicted_ratings, validation$rating)
RMSE_results <- RMSE_results %>% 
  add_row(Model = "baseline+b_i+b_u+b_g", 
          Desciption = "baseline with movie+user+genres effects", 
          RMSE_value = rmse)
RMSE_results


## ----Apply regularization to Baseline + movie effect + user effect, echo=TRUE, message=FALSE, warning=FALSE-----------------------------

lambdas <- seq(0, 10, 0.1)
rmses <- sapply(lambdas, function(l){
     Movie_effects_reg <- edx %>% group_by(movieId) %>% 
       summarize(b_i_reg = sum(rating - mu_hat)/(n()+l))
     User_effects_reg <- edx %>% left_join(Movie_effects_reg, by="movieId") %>% 
       group_by(userId) %>% 
       summarize(b_u_reg = sum(rating - mu_hat - b_i_reg)/(n()+l))
     
#Predicting Baseline + movie effect + user effect with regularization
     predicted_ratings <- 
          validation %>% 
          left_join(Movie_effects_reg, by = "movieId") %>%
          left_join(User_effects_reg, by = "userId") %>%
          mutate(pred = mu_hat + b_i_reg + b_u_reg) %>%
          pull(pred)
     return(RMSE(predicted_ratings, validation$rating))
})

qplot(lambdas, rmses)  


## ----Lambda value to Baseline + movie effect + user effect, echo=TRUE, message=FALSE, warning=FALSE-------------------------------------
tibble(Lambda = lambdas[which.min(rmses)], RMSE = min(rmses))


## ----RMSE of Baseline + movie effect + user effect with regularization, echo=TRUE, message=FALSE, warning=FALSE-------------------------
RMSE_results <- RMSE_results %>% 
  add_row(Model = "baseline+b_i_reg+b_u_reg", 
          Desciption = "Regularised with movie+user effects", 
          RMSE_value = min(rmses))
RMSE_results


## ----Apply regularization to Baseline + movie effect + user effect + genre effect, echo=TRUE, message=FALSE, warning=FALSE--------------

lambdas <- seq(0, 10, 0.1)
rmses <- sapply(lambdas, function(l){
     Movie_effects_reg <- edx %>% group_by(movieId) %>% 
       summarize(b_i_reg = sum(rating - mu_hat)/(n()+l))
     User_effects_reg <- edx %>% left_join(Movie_effects_reg, by="movieId") %>% 
       group_by(userId) %>% summarize(b_u_reg = sum(rating - mu_hat - b_i_reg)/(n()+l))
     Genre_effects_reg <- edx %>% left_join(Movie_effects_reg, by="movieId") %>% 
       left_join(User_effects_reg, by='userId') %>% group_by(genres) %>% 
     summarize(b_g_reg = sum(rating - mu_hat - b_i_reg - b_u_reg)/(n()+l))

# Predicting Baseline + movie effect + user effect + genre effect with regularization
     predicted_ratings <- 
          validation %>% 
          left_join(Movie_effects_reg, by = "movieId") %>%
          left_join(User_effects_reg, by = "userId") %>%
          left_join(Genre_effects_reg, by = "genres") %>%
          mutate(pred = mu_hat + b_i_reg + b_u_reg + b_g_reg) %>%
          pull(pred)
     return(RMSE(predicted_ratings, validation$rating))
})

qplot(lambdas, rmses)  


## ----Lambda value to Baseline + movie effect + user effect + genre effect, echo=TRUE, message=FALSE, warning=FALSE----------------------
tibble(Lambda = lambdas[which.min(rmses)], RMSE = min(rmses))


## ----RMSE Baseline + movie effect + user effect + genre effect with regularization, echo=TRUE, message=FALSE, warning=FALSE-------------
RMSE_results <- RMSE_results %>% 
  add_row(Model = "baseline+b_i_reg+b_u_reg+b_g_reg", 
          Desciption = "Regularised with movie+user+genres effects", 
          RMSE_value = min(rmses))
RMSE_results


## ----Convert training and validation datasets into Matrix, echo=TRUE, message=FALSE, warning=FALSE--------------------------------------
#Convert training and validation datasets into Matrix for processing
edx_MF <- edx %>% select(movieId, userId, rating)
edx_MF <- as.matrix(edx_MF)
validation_MF <- validation %>% select(movieId, userId, rating)
validation_MF <- as.matrix(validation_MF)


## ----Write to files, echo=TRUE, message=FALSE, warning=FALSE----------------------------------------------------------------------------
#Write to files
write.table(edx_MF, file = "edx_MF.txt", sep = " ", 
            row.names = FALSE, col.names = FALSE)
write.table(validation_MF, file = "validation_MF.txt", sep = " ", 
            row.names = FALSE, col.names = FALSE)


## ----Read from files, echo=TRUE, message=FALSE, warning=FALSE---------------------------------------------------------------------------
set.seed(123, sample.kind="Rounding")
edx_MF_set <- data_file("edx_MF.txt")
validation_MF_set <- data_file("validation_MF.txt")


## ----Create and optimize a Recommender Model, echo=TRUE, message=FALSE, warning=FALSE---------------------------------------------------
#Create a recommender object
recommender_obj <-Reco()

#optimization
optimized_values <- recommender_obj$tune(edx_MF_set, opts = 
                        list(dim = c(10, 20, 30), 
                        lrate = c(0.05, 0.1, 0.2), 
                        costp_l1 = 0, costq_l1 = 0, 
                        nthread = 6, niter = 10))
head(optimized_values)


## ----Train the Recommender Model, echo=TRUE, message=FALSE, warning=FALSE---------------------------------------------------------------
#training the recommender model
recommender_obj$train(edx_MF_set, opts = c(optimized_values$min, nthread = 6, niter = 20))


## ----Make prediction with trained Recommender Model, echo=TRUE, message=FALSE, warning=FALSE--------------------------------------------
# Make prediction on validation_MF_set:
Prediction_file <- tempfile()
recommender_obj$predict(validation_MF_set, out_file(Prediction_file))
Validation_ratings <- read.table("validation_MF.txt", header = FALSE, sep = " ")$V3
Predicted_ratings <- scan(Prediction_file)


## ----Compute the RMSE of the Recommender Model, echo=TRUE, message=FALSE, warning=FALSE-------------------------------------------------
#calculate RMSE
rmse <- RMSE(Predicted_ratings, Validation_ratings)
RMSE_results <- RMSE_results %>% 
          add_row(Model = "Matrix Factorization", 
          Desciption = "Matrix Factorization with parallel stochastic gradient descent", 
          RMSE_value = rmse)
RMSE_results

