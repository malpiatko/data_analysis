train.data <- list(class=train[,1], features=as.matrix(train[,2:data.length]))
test.data <- list(class=test[,1], features=as.matrix(test[,2:data.length]))

train.data$features[is.na(train.data$features)] <- 0
test.data$features[is.na(test.data$features)] <- 0

perceptron.train <- function(data, iter=5) {
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
  
  classified.all <- apply(pattern.aug, 1, function(x) x %*% weights <= 0)
  missclassified <- pattern.aug[which(classified.all),]
  for(i in 1:iter) {
    newWeights <- weights + colSums(missclassified)
    weights <- newWeights
  }
  weights
}

perceptron.classify <- function(train, test) {
  weights <-perceptron.train(train,100)
  classes <- apply(test$features, 1, function(x) perceptron.single(c(1,x), weights))
  count <- sum(as.numeric(classes==test$class))
  count
}

perceptron.single <- function(example, weights) {
  ifelse(example %*% weights > 0, 0, 1)
}