network <- function(data, H=3) {
  ##Initialising weights
  n.features <- ncol(data$features)
  n <- length(data$class)
  newclasses <- lapply(data$class, function(x) ifelse(x==0, 1, -1)
  alpha <- replicate(n.features+1, runif(H, 0, 0.1))
  omega <- runif(H + 1, -0.1, 0.1)
  pattern.aug <- cbind(rep(1, n), data$features)
  output <- network.forward(alpha, omega, pattern.aug)
  output
}

network.forward <- function(alpha, omega, examples) {
  apply(examples, 1, function(x) network.forward.single(alpha, omega, x))
}

network.forward.single <- function(alpha, omega, x) {
  hidden <- apply(alpha, 1, function(weight) weight%*% x)
  hidden <- c(1,hidden)
  omega%*%hidden
}

hyp.tangent <- function(x) {
  exp <- exp(x)
  (exp - 1)/(exp + 1) 
}