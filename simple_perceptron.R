train.clean <- na.aggregate(train, FUN=mean)
test.clean <- na.aggregate(test, FUN=mean)


train.data <- list(class=train.clean[,1], features=as.matrix(train.clean[,2:data.length]))
test.data <- list(class=test.clean[,1], features=as.matrix(test.clean[,2:data.length]))

perceptron.train <- function(data) {
  n <- length(data$class)
  feat.n <- ncol(data$features)
  pattern.aug <- cbind(rep(1, n), data$features)
  weights <- rep(0,feat.n + 1)
  
  ##Invert weights for class 1
  for(i in 1:n) {
    if(data$class[i]==1) {
      pattern.aug[i] <- -pattern.aug[i]
    }
  }
  
  data$missclassified <- apply(pattern.aug, 1, function(x) x %*% weights <= 0)
  missclassified <- data[which(data$missclassified)]
  missclassified
}


criterion <- function(pattern, weights) {
  
}