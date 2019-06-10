rm(list = ls())

library(recommenderlab)
library(recosystem)
library(data.table)
library(RColorBrewer)
library(ggplot2)
require(devtools)

library("recommenderlab")
library(Matrix)
matrix <-read.csv("pivot - decimal.csv")
matrix <-read.csv("binary pivot.csv")

matrix[is.na(matrix)] <- 0
matrix <- data.matrix(matrix, rownames.force = NA)
matrix <- subset(matrix, select = -c(X))

sparse_ratings <- Matrix(matrix, sparse = TRUE)  
#sparse_ratings <- as(matrix, "dgCMatrix")
real_ratings <- new("realRatingMatrix", data = sparse_ratings)
real_ratings

#models
#popular
model <- Recommender(real_ratings, method = "POPULAR", param=list(normalize = "center"))
prediction <- predict(model, real_ratings[1:5], type="ratings")
as(prediction, "matrix")[,1:5]
set.seed(1)
e <- evaluationScheme(real_ratings, method="split", train=0.8, given=1)
model <- Recommender(getData(e, "train"), "POPULAR")
prediction <- predict(model, getData(e, "known"), type="ratings")
rmse_popular <- calcPredictionAccuracy(prediction, getData(e, "unknown"))[1]
rmse_popular

#UBCF
model <- Recommender(real_ratings, method = "UBCF", 
                     param=list(normalize = "z-score", method="Cosine", nn=50))

model <- Recommender(real_ratings, method = "UBCF", 
                     param=list(normalize = "center", method="Euclidean", nn=50))
#method z-score, pearson
prediction <- predict(model, real_ratings[1:5], type="ratings")
as(prediction, "matrix")[,1:5]
set.seed(1)
prediction <- predict(model, getData(e, "known"), type="ratings")
rmse_ubcf <- calcPredictionAccuracy(prediction, getData(e, "unknown"))[1]
rmse_ubcf

#IBCF
model <- Recommender(real_ratings, method = "IBCF", 
                     param=list(normalize = "center", method="pearson", k=20))
prediction <- predict(model, real_ratings[1:5], type="ratings")
as(prediction, "matrix")[,1:8]
prediction <- predict(model, getData(e, "known"), type="ratings")
rmse_ubcf <- calcPredictionAccuracy(prediction, getData(e, "unknown"))[1]
rmse_ubcf








ratings_data <-read.csv("ratings data.csv")
ratings_data <-read.csv("binary rating.csv")
smp_size <- floor(0.9 * nrow(ratings_data))
train_ind <- sample(1: nrow(ratings_data), size = smp_size)
train <- ratings_data[train_ind, ]
test <- ratings_data[-train_ind, ]
train_data <- data_memory(user_index = train$user, item_index = train$item, 
                          rating = train$rating, index1 = T)
test_data <- data_memory(user_index = test$user, item_index = test$item, 
                         rating = test$rating, index1 = T)
recommender <- Reco()
recommender$train(train_data, opts = c(dim = 100, costp_l2 = 0.1, costq_l2 = 0.1, 
                                       lrate = 0.1, niter = 100, nthread = 6, verbose = F)) 
test$prediction <- recommender$predict(test_data, out_memory())
test$prediction
