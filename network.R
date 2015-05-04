network <- function(data, H=3) {
  ##Initialising weights
  n.features <- ncol(data$features)
  n <- length(data$class)
  ##Initialising weights. We add additional value for the offset weight
  ## a[i,j] - from hidden i to input
  alpha <- replicate(n.features+1, runif(H, -0.1, 0.1))
  omega <- runif(H + 1, -0.1, 0.1)
  weights <- list(level1=alpha, level2=omega)
  
  
  pattern.aug <- cbind(rep(1, n), data$features)
  output1 <- output.level1(weights$level1, pattern.aug)
  output2 <- output.level2(weights$level2, cbind(rep(1,n),output1))
  output
}

##Computes output of first layer for each example
output.level1 <- function(weights, examples) {
  t(apply(examples, 1, function(x) output1.example(weights, x)))
}

##Computed output of first layer for given example
output1.example <- function(weights, x) {
  as.vector(apply(weights,1, function(a) sygmoid(a %*% x)))
}

output.level2 <- function(weights, examples) {
  apply(examples, 1, function(x) weights %*% x)
}



network.forward <- function(alpha, omega, examples) {
  apply(examples, 1, function(x) network.forward.single(alpha, omega, x))
}

network.forward.single <- function(alpha, omega, x) {
  hidden <- apply(alpha, 1, function(weight) weight%*% x)
  hidden <- c(1,hidden)
  omega%*%hidden
}

sygmoid <-function(x) {
  1/(1+exp(-x))
}

hyp.tangent <- function(x) {
  exp <- exp(x)
  (exp - 1)/(exp + 1) 
}