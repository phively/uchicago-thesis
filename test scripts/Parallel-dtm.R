# Libraries
library(foreach)
library(doParallel)
library(wranglR)
library(topicmodels)
library(slam)

# Setup
registerDoParallel(cores=3)

# Test dtm data
source("R/function-split_set.R")
load("data/sparse_test.zip")
K <- 3
set.seed(123)
inds <- KFoldXVal(1:sparse_test$nrow, k = K)

# No threading
print("%do%")
now <- Sys.time()
r <- foreach(i = 1:K) %do% {
  # Data to use
  this_ind <- unlist(inds)[unlist(inds) %nin% inds[[i]]]
  this_data <- split_set(sparse_test, this_ind)
  # Create a topic model
  return(
    LDA(this_data, k = 5, method = "VEM", control = list(seed = 123))
  )
}
print(Sys.time() - now)

# Threading
print("%dopar%")
now <- Sys.time()
r <- foreach(i = 1:K, .packages = c("wranglR", "topicmodels", "slam")) %dopar% {
  # Data to use
  this_ind <- unlist(inds)[unlist(inds) %nin% inds[[i]]]
  this_data <- split_set(sparse_test, this_ind)
  # Create a topic model
  return(
    LDA(this_data, k = 5, method = "VEM", control = list(seed = 123))
  )
}
print(Sys.time() - now)