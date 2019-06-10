rm(list = ls())
library(data.table)
library(dplyr)
library(recosystem)


ratings_data <-read.csv("ratings data.csv")
ratings_data <-read.csv("categorized rating.csv")
ratings_data <- read.csv("binary rating.csv")
tickers <-read.csv("unique tickers.csv")

smp_size <- floor(0.9 * nrow(ratings_data))
train_ind <- sample(1: nrow(ratings_data), size = smp_size)
train <- ratings_data[train_ind, ]
test <- ratings_data[-train_ind, ]
train_data <- data_memory(user_index = train$user, item_index = train$item, 
                          rating = train$rating, index1 = T)
test_data <- data_memory(user_index = test$user, item_index = test$item, 
                         rating = test$rating, index1 = T)
recommender <- Reco()
recommender$train(train_data, opts = c(dim = 30, costp_l2 = 0.1, costq_l2 = 0.1, 
                                       lrate = 0.1, niter = 100, nthread = 6, verbose = F)) 
test$prediction <- recommender$predict(test_data, out_memory())
head(test)
RMSE <- function(true_ratings, predicted_ratings){
  sqrt(mean((true_ratings - predicted_ratings)^2))
}
RMSE(test$rating, test$prediction)







