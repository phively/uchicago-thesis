# Libraries
library(foreach)
library(doParallel)
library(wranglR)
library(topicmodels)
library(slam)

# Setup
registerDoParallel(cores=3)

# Iris data
x <- iris[which(iris[,5] != "setosa"), c(1,5)]
trials <- 5000

# No thread
print("%do%")
ptime <- system.time({
  r <- foreach(icount(trials), .combine=cbind) %do% {
    ind <- sample(100, 100, replace=TRUE)
    result1 <- glm(x[ind,2]~x[ind,1], family=binomial(logit))
    coefficients(result1)
  }
})[3]
print(ptime)

# Thread
print("%dopar%")
ptime <- system.time({
  r <- foreach(icount(trials), .combine=cbind) %dopar% {
    ind <- sample(100, 100, replace=TRUE)
    result1 <- glm(x[ind,2]~x[ind,1], family=binomial(logit))
    coefficients(result1)
  }
})[3]
print(ptime)
