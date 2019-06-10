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
matrix[is.na(matrix)] <- 0
matrix <- data.matrix(matrix, rownames.force = NA)
matrix <- subset(matrix, select = -c(X))

sparse_ratings <- Matrix(matrix, sparse = TRUE)  
#sparse_ratings <- as(matrix, "dgCMatrix")
real_ratings <- new("realRatingMatrix", data = sparse_ratings)
real_ratings


#################################################################
library(irlba)
similarity_users <- similarity(real_ratings[1:50,], 
                               method = "cosine", 
                               which = "users")

image(as.matrix(similarity_users), main = "User similarity")
similarity_movies <- similarity(real_ratings[,1:50], 
                                method = "cosine", 
                                which = "items")

image(as.matrix(similarity_movies), main = "Ticker similarity")
set.seed(1)
Y <- irlba(sparse_ratings,tol=1e-4,verbose=TRUE,nv = 100, maxit = 1000)
# plot singular values
plot(Y$d, pch=20, col = "blue", cex = 1.5, xlab='Singular Value', ylab='Magnitude', 
     main = "Singular Values for User-Item Matrix")
# calculate sum of squares of all singular values
all_sing_sq <- sum(Y$d^2)
# variability described by first 6, 12, and 20 singular values
first_six <- sum(Y$d[1:6]^2)
print(first_six/all_sing_sq)
first_20 <- sum(Y$d[1:20]^2)
print(first_20/all_sing_sq)
perc_vec <- NULL
for (i in 1:length(Y$d)) {
  perc_vec[i] <- sum(Y$d[1:i]^2) / all_sing_sq
}

plot(perc_vec, pch=20, col = "blue", cex = 1.5, xlab='Singular Value', ylab='% of Sum of Squares of Singular Values', main = "Choosing k for Dimensionality Reduction")
lines(x = c(0,100), y = c(.90, .90))
k = length(perc_vec[perc_vec <= .90])
k
U_k <- Y$u[, 1:k]
dim(U_k)
D_k <- Diagonal(x = Y$d[1:k])
dim(D_k)
V_k <- t(Y$v)[1:k, ]
dim(V_k)

min_n_items <- quantile(rowCounts(real_ratings), 0.9)
print(min_n_items)
min_n_users <- quantile(colCounts(real_ratings), 0.9)
print(min_n_users)
ratings_items <- real_ratings[rowCounts(real_ratings) > min_n_items,
                              colCounts(real_ratings) > min_n_users]
ratings_items
#i define the RMSE function as:
RMSE <- function(true_ratings, predicted_ratings){
  sqrt(mean((true_ratings - predicted_ratings)^2))
}






