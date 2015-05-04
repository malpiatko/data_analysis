train.data <- list(class=train[,1], features=as.matrix(train[,2:data.length]))
test.data <- list(class=test[,1], features=as.matrix(test[,2:data.length]))

train.data$features[is.na(train.data$features)] <- 0
test.data$features[is.na(test.data$features)] <- 0

perceptron.train <- function(data, iter) {
  n <- length(data$class)
  feat.n <- ncol(data$features)
  pattern.aug <- cbind(rep(1, n), data$features)
  weights <- rep(0, feat.n + 1)
  
  ##Invert weights for class 1
  for(i in 1:n) {
    if(data$class[i]==1) {
      pattern.aug[i,] <- -pattern.aug[i,]
    }
  }
  
  for(i in 1:iter) {
    ##Classify, return true if missclassified.
    classified.all <- apply(pattern.aug, 1, function(x) (x %*% weights) > 0)
    ##Take the subset of examples which were missclassified
    missclassified <- pattern.aug[which(!classified.all),]
    print(nrow(missclassified))
    if(!is.null(dim(missclassified))) {
      weights <- weights + 0.1*colSums(missclassified)
    } else {
      break
    }
  }
  weights
}

perceptron.train.iter <-function(data, iter) {
  n <- length(data$class)
  feat.n <- ncol(data$features)
  pattern.aug <- cbind(rep(1, n), data$features)
  weights <- as.vector(rep(0, feat.n + 1))
  
  ##Invert weights for class 1
  for(i in 1:n) {
    if(data$class[i]==1) {
      pattern.aug[i,] <- -pattern.aug[i,]
    }
  }
  
  for(i in 1:iter) {
    for(j in 1:n) {
      weights <- perceptron.iter(pattern.aug[j,], weights)
    }
  }
  weights
}

perceptron.iter <- function(example, weights) {
  if(example %*% weights <= 0) {
    weights + example
  } else {
    weights
  }
}

perceptron.classify <- function(train, test, iter=100) {
  weights <-perceptron.train.iter(train,iter)
  classes <- apply(test$features, 1, function(x) perceptron.single(c(1,x), weights))
  count <- sum(as.numeric(classes==test$class))
  count
}

perceptron.single <- function(example, weights) {
  ifelse(example %*% weights > 0, 0, 1)
}