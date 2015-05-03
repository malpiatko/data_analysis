require(class)
install.packages("zoo", type="source", repos="http://cran.stat.ucla.edu/")
data9 <- read.table("http://wwwf.imperial.ac.uk/~eakc07/S7/data9.dat")

## Set up constants
data.size <- nrow(data9)
data.length <- ncol(data9)
test.size <- 100
train.size <- data.size - test.size

##Split data into train/test
# set.seed(123)
# train_ind <- sample(seq_len(data.size), size = train.size)
# 
# train <- data9[train_ind, ]
# test <- data9[-train_ind, ]

#Split data into train/test
set.seed(1234)
train_ind <- sample(seq_len(data.size), size = train.size)

train <- data9[train_ind, ]
test <- data9[-train_ind, ]



## KNN

## Normalize data
normalize <- function(x) {
  return ((x- min(x, na.rm = TRUE))/ (max(x, na.rm = TRUE) - min(x, na.rm = TRUE)))
}

train_norm <- as.data.frame(lapply(train[,2:data.length], normalize))
train_target <- train[,1]
test_norm <- as.data.frame(lapply(test[,2:data.length], normalize))
test_target <- test[,1]

K <- sqrt(data.length)

## For given training data and target vector, it outputs the predicted class for example
knn.single <- function(train, target, example, k = K) {
  d <- apply(train,1, function(point) dist(rbind(point, example), method="euclidean"))
  d.sorted <- (sort.int(d,index.return=TRUE))
  neighbour.id <- d.sorted$ix[1:k]
  neighbour.distance <- d.sorted$x[1:k]
  ##knn.getClass.normal(neighbour.id, target, k)
  knn.getClass.kernel(neighbour.id, target, neighbour.distance, k)
}

knn.getClass.normal <- function(neighbour.id, target, k = K) {
  class0 <- 0
  for(id in 1:k) {
    class0 <- class0 + target[neighbour.id[id]]
  }
  if(class0 > k - class0) {
    1
  } else {
    0
  }
}

tricube <- function(x) {
  return ((1-abs(x)^3)^3)
}

knn.getClass.kernel <- function(neighbour.id, target, neighbour.distance, k = K) {
  score0 <- 0
  score1 <- 0
  neighbour.distance <- neighbour.distance/neighbour.distance[k]
  for(i in 1:k) {
    if(target[neighbour.id[i]] == 0) {
      score0 <- score0 + tricube(neighbour.distance[i])
    } else {
      score1 <- score1 + tricube(neighbour.distance[i])
    }
  }
  if(score0 > score1) {
    0
  } else {
    1
  }
}

knn.classify <- function(train, train.target, test, test.target, k = K) {
  classes <- apply(test, 1, function(row) knn.single(train, train.target, row, k))
  count <- sum(as.numeric(classes==test.target))
  count
}
