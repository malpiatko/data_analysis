train.clean <- na.aggregate(train, FUN=mean)
test.clean <- na.aggregate(test, FUN=mean)

train.data <- list(class=train.clean[,1], features=as.matrix(train.clean[,2:data.length])) 
test.data <- list(class=test.clean[,1], features=as.matrix(test.clean[,2:data.length]))

perceptron.train <- function(data) {
  data
}
