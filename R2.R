#506
getwd()
setwd("/Users/dalaohuhan/Documents/GitHub/assignment2")
cars_data <- read.csv("cars.csv")
names(cars_data) <- c("height",
                 "length",
                 "width",
                 "driveline",
                 "engine_type",
                 "hybrid",
                 "gears",
                 "trans",
                 "city_mpg",
                 "fuel_type",
                 "hw_mpg",
                 "id_class",
                 "id",
                 "id_make",
                 "model_year",
                 "id_year",
                 "horsepower",
                 "torque")
cars <- cars_data[which(cars_data$fuel_type == "Gasoline"),]


play_dice <- function(n) {
  r <- -2
  for (i in 1:n) {
    d <- sample(1:6, 1, replace = TRUE)
    if (d == 1 | 3 |5) {
      r = r + 0
    }
    if (d == 2) {
      r = r + 2
    }
    if (d == 4) {
      r = r + 4
    }
    if (d == 6) {
      r = r + 6
    }
  }
  return(r)
}
play_dice(1)
###1
play_dice1 <- function(n) {
  r <- 0
  for (i in 1:n) {
    d <- sample(1:6, 1, replace = TRUE)
    if (d == 1 | d == 3 | d == 5) {
      r <- r - 2
    }
    if (d == 2) {
      r <- r - 2 + 2
    }
    if (d == 4) {
      r <- r - 2 + 4
    }
    if (d == 6) {
      r <- r - 2 + 6
    }
  }
  return(r)
}

apply()
install.packages("RCPP")
n <- 10
x <-sample(1:6, n, replace = TRUE)

###2
play_dice2 <- function(n){
  x <-sample(1:6, n, replace = TRUE)
  for (i in 1:length(x))
  if (x[i] == 1 | x[i] == 3 | x[i] == 5) {
    x[i] <- -2
  } else {
    x[i] <- x[i] - 2
  }
  return(sum(x))
}
play_dice2(3000)

###3
play_dice3 <- function(n){
  x <-sample(1:6, n, replace = TRUE)
  m <- as.data.frame(table(x))
  m1 <- as.numeric(m[1,])
  m1_1 <-
  m2 <- as.numeric(m[2,])
  t <- (t(m1)%*%m2)
  return(t)
}
play_dice3(3000)
table(play_dice3(n))
n <- 3000
library(microbenchmark)

microbenchmark(play_dice1(n), play_dice2(n))

m <- table(c(3,6,6,2,6,1,2,3,4,5,2,4))
m3 <- as.data.frame(m)
m5 <- as.matrix(m)
-2*m5[m5[1,1]] -2*m5[m5[3,1]] + 2*m5[m5[4,1]] -2*m5[m5[5,1]] + 4*m5[m5[6,1]]
?solve

###3
play_dice3 <- function(n){
  x <-sample(1:6, n, replace = TRUE)
  for (i in 1:length(x))
    if (x[i] == 1 | x[i] == 3 | x[i] == 5) {
      x[i] <- -2
    } else {
      x[i] <- x[i] - 2
    }
  m <- as.data.frame(table(x))
  if (length(which(m$x == -2)) == 0) {
    f_2_ <- 0
  } else {
    f_2_ <- as.numeric(m[which(m$x == -2),2])
  }
  
  if (length(which(m$x == 0)) == 0) {
    f_0 <- 0
  } else {
    f_0 <- as.numeric(m[which(m$x == 0),2])
  }
  if (length(which(m$x == 2)) == 0) {
    f_2 <- 0
  } else {
    f_2 <- as.numeric(m[which(m$x == 2),2])
  }
  
  if (length(which(m$x == 4)) == 0) {
    f_4 <- 0
  } else{
    f_4 <- as.numeric(m[which(m$x == 4),2])
  }
  r <- -2*f_2_ + 2*f_2 + 4*f_4
  return(r)
}
play_dice3(1)



###4

myfun <- function(d){
  d <-sample(1:6, 1, replace = TRUE)
  if (d == 1 | d == 3 | d == 5) {
    r <- -2
  }
  if (d == 2) {
    r <- 0
  }
  if (d == 4) {
    r <- 2
  }
  if (d == 6) {
    r <- 4
  }
  return(r)
}
myfun(1)

play_dice4 <- function(n) {
  f <- 1:n
  p <- sapply(f, function(d){
    d <-sample(1:6, 1, replace = TRUE)
    if (d == 1 | d == 3 | d == 5) {
      r <- -2
    }
    if (d == 2) {
      r <- 0
    }
    if (d == 4) {
      r <- 2
    }
    if (d == 6) {
      r <- 4
    }
    return(r)
  })
  return(sum(p))
}
play_dice4(3)
microbenchmark(play_dice1(3000),play_dice2(3000),play_dice3(3000))
n <- 3
mat <- matrix(c(-2,2,4,f_2_,f_2,f_4), ncol = 2)

