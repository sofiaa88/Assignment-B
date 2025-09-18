
  

##Exercises 1: VECTORS 
#Problem 1a
onea <- 1:20
onea

#1b
oneb <- 20:1
oneb

#1c
onec <- c(1:20,19:1)
onec

#1d
tmp <- matrix(c(4,6,3), nrow=1)
tmp

#1e
?rep
onee <- rep(tmp, 10)
onee
length(which(e==4))

#1f
onef <- rep(tmp,l=31)
length(which(onef==4))
length(which(onef==6))
length(which(onef==3))

#1g
oneg <- rep(tmp, times=c(10,20,30))
oneg

#Problem 2
library(base)
library(graphics)
a <- seq(3,6, by=0.1)
a
twoa <- exp(a)*cos(a)
twoa

#Problem 3a
a <- rep(0.1)
b <- rep(0.2)
x <- seq(3,36, by=3)
y <- seq(1,34, by=3)
threea <- a^x*b^y
threea

#3b
a <- rep(2)
b <- seq(1,25, by=1)
threeb <- (a^b)/b
threeb

#Problem 4a
a <- 10:100
foura <- (a^3)+(4*a^2)
foura

#4b
b = 1:25
fourb <- ((2^b)/b)+(3^b)/(b^2)
fourb

# Problem 5a
x <- 1:30
words <- paste("label ", x) #maddy helped me
words

#5b
words1 <- paste0("fn", x)
words1

#Problem 6
set.seed(50)
xVec <- sample(0:999, 250, replace = T)
yVec <- sample(0:999, 250, replace = T)

#6a
s <- seq(1:n)
sixa <- yVec[s+1]- xVec[s]
sixa

#6b
sixb <- seq((sin(yVec[s])/(cos(xVec[s+1]))))
sixb

#6c
sixc <- seq(xVec[s]+2*xVec[s+1]-xVec[s+2])
sixc

#6d
i <- 1
n <- 1:n  
sixd <- (e^-x[i+1])/(x[i]+10)
sixd

##Exercises 2: MATRICES
## Problem 1a 
A <- matrix(c(1,5,-2,1,2,-1,3,6,-3), nrow = 3)
A
A %*% A %*% A 
  
## Problem 1b
A[,3] <- A[,2] + A[,3]
A

## Problem 2
B <- matrix(c(10,-10,10), byrow = TRUE, ncol = 3, nrow = 15)

P1 <- t(B)%*%B
P2 <- crossprod(B)
P1 == P2

## Prob 3
matE <- matrix(0,nr=6,nc=6)
?col()
col(matE)
row(matE)

col(matE) - row(matE)

abs(col(matE)-row(matE))==1

matE[abs(col(matE)-row(matE))==1] <- 1

#Prob 4
?outer
outer(0:4,0:4,"+")

#Prob 5a

#5b

#5c


#Prob 6
y <- c(7,-1,-3,5,17)
A <- matrix(0,nr=5, nc=5)
A <- abs(col(A)-row(A))+1
A

x <- solve(A,y)

A%*%x

# Prob 7


# Prob 8a

639215

##Exercises 3: SIMPLE FUNCTIONS
#Problem 1a: # help from chatgpt
tmpFn1 <- function(xVec) {
  n <- length(xVec)
  xVec ^ (1:n)
}
tmpFn2 <- function(xVec) {
  n <- length(xVec)
  (xVec ^ (1:n)) / (1:n)
}

xVec <- c(2, 3, 4)
tmpFn1(xVec)  
tmpFn2(xVec) 

#1b: #help from chatgpt
tmpFn3 <- function(x, n) {
  if(n < 1) stop("n must be a positive integer")
  terms <- sapply(2:n, function(k) x^k / k)  # vector of x^k / k for k = 2 to n
  result <- 1 + sum(terms)  # add the initial 1
  return(result)
}
tmpFn3(2, 4)

#Problem 2
a <- c(1:5,6:1)
n <- length(a)

a[-c(n-1, n)]
a[-c(1, n)]
a[-c(1, 2)]

a[-c(n-1,n)] + a[-c(1,n)] + a[-c(1,2)]
(a[-c(n-1,n)] + a[-c(1,n)] + a[-c(1,2)])/3

tmpFn <- function(a){
  n = length(a)
  mva= (a[-c(n-1,n)] + a[-c(1,n)] + a[-c(1,2)])/3
  return(mva)
}

tmpFn(a)

#Problem 3


#Problem 4 
A <- matrix(c(1,1,3,5,2,6,-2,-1,-3), nrow=3, byrow=T)
A%%2
A%%2 == 1
A[A%%2 == 1]

bb <- 2*A[A%%2 == 1]
A[A%%2 == 1] <- 2*A[A%%2 == 1]
A
is.matrix(A)

tmpFn <- function(mat){
  if(!is.matrix(mat)) stop("Input is not a matrix")
  mat[mat%%2 == 1] <- 2*mat[mat%%2 == 1]
  return(mat)
}

A <- matrix(c(1,1,3,5,2,6,-2,-1, -3), nrow = 3, byrow = T)

tmpFn(A)

#Problem 5a

#5b

#5c

#Problem 8 
testloop <- function(n){
  if(n<4)stop("n must be an integer > 3")
  browser()
  x = rep(NA, n-1)
  x[1] = 1
  x[2] = 2
  
  for(j in 3:(n-1)){x[j] = x[j-1] + 2/x[j-1]
  }
  return(x)
}

testloop(5)

##Exercises 4: HARDER FUNCTIONS
#Problem 1a
set.seed(50)
x <- as.integer(runif(5, 1, 5))
y <- as.integer(runif(6, 2, 4))
?as.integer()
z <- outer(y, x, "<")
colSums(z)

f_1a <- function(x, y){
  z = colSums(outer(y,x,"<"))
  return(z)
}

f_1a(x,y)
rowSums(sapply(y, FUN=function(y){y < x}))

#1b
f_1b <- function(x,y){
  rowSums(sapply(y, FUN=function(y){y < x}))
}

f_1b(x,y)

#1c
f_1c <- function(x,y){
  
  rowSums(vapply(y, FUN=function(y){y<x}, 
                 FUN.VALUE = (along=x)))
}

f_1c(x,y)
#1d

#1e
set.seed(53)
x1 <- rnorm(10010)
y1 <- rnorm(10020)

system.time(f_1a(x1,y1))

system.time(f_1b(x1,y1))

system.time(f_1c(x1,y1))

#Problem 2a
noNA <- function(mat) {mat[, colSums(is.na(mat))==0, drop= FALSE]}

#2b
removeNArowscols <- function(mat) {
  keep_rows <- rowSums(is.na(mat)) == 0
  keep_cols <- colSums(is.na(mat)) == 0
  mat[keep_rows, keep_cols, drop = FALSE]
}

#Problem 5a
aRate <-
  
sRate <- 

# simulation of queue
queue <- function(n, aRate, sRate) {
  # generate random interarrival and service times
  A <- rexp(n, rate = aRate)   # interarrival times
  S <- rexp(n, rate = sRate)   # service times
  
  # vector to hold waiting times W0,...,Wn
  W <- numeric(n + 1)          
  W[1] <- 0  # W0 = 0
  
  # recurrence
  for (j in 1:n) {
    W[j + 1] <- max(0, W[j] + S[j] - A[j+1])
  }
  
  W[n + 1]   # return Wn
}

set.seed(1)
queue(50, 2, 2)

#5b
queueLoop <- function(n, aRate, sRate, reps = 1000) {
  out <- numeric(reps)
  for (i in 1:reps) {
    out[i] <- queue(n, aRate, sRate)
  }
  out
}
#repetition
queueRep <- function(n, aRate, sRate, reps = 1000) {
  replicate(reps, queue(n, aRate, sRate))
}

#comparing times
system.time(queueLoop(50, 2, 2, 10000))
system.time(queueRep(50, 2, 2, 10000))

#5c
queueVec <- function(n, aRate, sRate) {
  D <- rexp(n, rate = sRate) - rexp(n, rate = aRate)  # increments (service - arrival)
  
  W <- Reduce(
    function(prev, d) max(0, prev + d),
    D, init = 0, accumulate = TRUE
  )
  
  tail(W, 1)  # return Wn
}
system.time(replicate(10000, queueVec(50, 2, 2)))

#Problem 6
set.seed(123) 
# rwalk(n): returns positions S0,...,Sn
rwalk <- function(n) {
  steps <- sample(c(-1, 1), n, replace = TRUE, prob = c(0.5, 0.5))
  positions <- c(0, cumsum(steps))  # include S0=0
  return(positions)
}

rwalk(10)

#Simulate the 50 steps 

set.seed(123)
n <- 50
path <- rwalk(n)

plot(0:n, path, type = "o", pch = 16, col = "blue",
     xlab = "Step", ylab = "Position",
     main = "Random Walk (Single Path)")
abline(h = 0, col = "red", lty = 2)  # x-axis


##Exercises 5: DATA FRAME, LIST, ARRAY, AND TIME SERIES
#Problem 1
install.packages("TSstudio")
library(TSstudio)

set.seed(50)
tmp <- ts(rnorm(4000), start= c(1960,3), frequency= 12)

ts_info(tmp)
ts_plot(tmp)

hist(tmp)

#Problem 4a
testFn <- function(arr) {
  d1 <- dim(arr)[1]  # rows
  d2 <- dim(arr)[2]  # columns
  d3 <- dim(arr)[3]  # layers
  
  # w_{i,j,k} = x_{i,j,k} - min_i x_{i,j,k} for each column/layer
  w <- array(0, dim=c(d1,d2,d3))
  for(k in 1:d3){
    for(j in 1:d2){
      w[,j,k] <- arr[,j,k] - min(arr[,j,k])
    }
  }
  
  # z_{j,k} = sum_i x_{i,j,k} - max_i x_{i,j,k} for each column/layer
  z <- matrix(0, nrow=d2, ncol=d3)
  for(k in 1:d3){
    for(j in 1:d2){
      z[j,k] <- sum(arr[,j,k]) - max(arr[,j,k])
    }
  }
  
  return(list(w=w, z=z))
}

#4b
testFn2 <- function(arr) {
  d1 <- dim(arr)[1]
  d2 <- dim(arr)[2]
  d3 <- dim(arr)[3]
  
  z <- matrix(0, nrow=d2, ncol=d3)
  for(k in 1:d3){
    for(j in 1:d2){
      z[j,k] <- sum(arr[,j,k])
    }
  }
  return(z)
}

# Example
set.seed(123)
testArray <- array(sample(1:60, 60, replace=FALSE), dim=c(5,4,3))
res1 <- testFn(testArray)
res2 <- testFn2(testArray)

testArray
res1$w  # 3D array w
res1$z  # 2D matrix z
res2     # 2D matrix z

#Problem 2a #help from chatgpt on Problem 2
myListFn <- function(n) {
  xVec <- rnorm(n)
  xMean <- mean(xVec)
  
  if (xMean >= 0) {
    yVec <- rexp(n, rate = 1/xMean)  
  } else {
    zVec <- rexp(n, rate = 1/(-xMean))  
    yVec <- -zVec
  }
  
  count <- sum(abs(yVec) > abs(xVec))
  
  return(list(xVec = xVec, yVec = yVec, count = count))
}

#2b
lapply(rep(10, 4), myListFn)
sapply(rep(10, 4), myListFn)

replicate(4, myListFn(10), simplify = FALSE)

#2c
set.seed(123)
myList <- lapply(1:1000, function(i) myListFn(10))
yList <- lapply(myList, `[[`, "yVec")  
length(yList)  

#2d
yMat <- sapply(myList, `[[`, "yVec")  
dim(yMat)
  
#2e
myListNoCount <- lapply(myList, function(lst) {
  lst$count <- NULL
  return(lst)
})

#2f
myListCountGT2 <- Filter(function(lst) lst$count > 2, myList)
length(myListCountGT2)

#Problem 3a #help from chatgpt on Problem 3
weights <- 1:10

weighted_sum <- function(vec) sum(vec * weights)

xSums <- sapply(myList, function(lst) weighted_sum(lst$xVec))
ySums <- sapply(myList, function(lst) weighted_sum(lst$yVec))

length(xSums)  
length(ySums)  

#3b
xMat <- sapply(myList, `[[`, "xVec")  
yMat <- sapply(myList, `[[`, "yVec") 

# Compute differences
diffMat <- t(xMat - yMat) 
dim(diffMat)

#3c
x2 <- xMat[2, ]  
y2 <- yMat[2, ] 

iVec <- 1:1000
sum_i_x2 <- sum(iVec * x2)
sum_i_y2 <- sum(iVec * y2)

sum_i_x2
sum_i_y2

#Problem 4a #help from chatgpt on Problem 4
testFn <- function(arr) {
  wArray <- arr - min(arr)
  
  zMat <- apply(arr, c(2,3), sum) - max(arr)
  
  return(list(wArray = wArray, zMat = zMat))
}

set.seed(123)
testArray <- array(sample(1:60, 60, replace=F), dim=c(5,4,3))
result <- testFn(testArray)

result$wArray  
result$zMat   

#4
testFn2 <- function(arr) {
  d1 <- dim(arr)[1]
  d2 <- dim(arr)[2]
  d3 <- dim(arr)[3]
  
  zMat <- matrix(0, nrow = d2, ncol = d3)  # d2 x d3
  
  for (k in 1:d3) {
    zMat[, k] <- apply(arr[,,k]^k, 2, sum)
  }
  
  return(zMat)
}

zOnly <- testFn2(testArray)
dim(zOnly) 
zOnly









