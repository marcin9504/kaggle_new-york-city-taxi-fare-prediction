library(readr)
library(rpart)
library(RSNNS)

standarise <- function(column){
  result <- (column - mean(column))/sd(column)
  return(result)
}

train <- read_csv(file="R/new-york-city-taxi-fare-prediction/train_small.csv")

train$taxi_distance <- abs(train$pickup_longitude - train$dropoff_longitude) + abs(train$pickup_latitude - train$dropoff_latitude)

train$taxi_distance <- standarise(train$taxi_distance)
train$passenger_count <- standarise(train$passenger_count)

train$pickup_latitude <- standarise(train$pickup_latitude)
train$pickup_longitude <- standarise(train$pickup_longitude)
train$dropoff_latitude <- standarise(train$dropoff_latitude)
train$dropoff_longitude <- standarise(train$dropoff_longitude)

x <- train[,4:9]
y <- train[,2]
model <- mlp(x, y,maxit=1000,learnFunc="Rprop",linOut=TRUE)

summary(model)

predictions <- predict(model, x)
a <- predictions[,1]
b <- sapply(y, as.numeric)[,1]
plotRegressionError(a, b)



