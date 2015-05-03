perceptron.train <- function(feature, classes) {
  weights <- 
}

eliminate.na <- function(features) {
  mean <- colMeans(features)
  for(col.id in 1: length(features)) {
    apply
  }
}