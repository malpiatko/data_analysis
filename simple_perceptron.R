perceptron.train <- function(features, classes) {
  features.clean <- aggregate(.~V2, features, FUN=mean)
  features.clean
}
