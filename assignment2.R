
  ## Problem 1-Dice Game
  
  #a. 
  
  #version 1: Implement this game using a loop over the die rolls.

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

#version 2: Implement this game using built-in R vectorized functions.

play_dice2 <- function(n) {
  x <-sample(1:6, n, replace = TRUE)
  for (i in 1:n)
    if (x[i] == 1 | x[i] == 3 | x[i] == 5) {
      x[i] <- -2
    } else {
      x[i] <- x[i] - 2
    }
  return(sum(x))
}

#version 3: Implement this by collapsing the die rolls into a single table(). 

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

#version 4: Implement this game by using one of the “apply” functions.

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

#b.
#Demonstrate that all versions work. 

play_dice1(3)
play_dice1(3000)
play_dice2(3)
play_dice2(3000)
play_dice3(3)
play_dice3(3000)
play_dice4(3)
play_dice4(3000)


#c.
#Demonstrate that the four versions give the same result.Test with inputs 3 and 3000. 

set.seed(1)
play_dice1(3)
set.seed(1)
play_dice2(3)
set.seed(1)
play_dice3(3)
set.seed(1)
play_dice4(3)

set.seed(2)
play_dice1(3000)
set.seed(2)
play_dice2(3000)
set.seed(2)
play_dice3(3000)
set.seed(2)
play_dice4(3000)

#d.
#Use the microbenchmark package to clearly demonstrate the speed of the implementations. Compare performance with a low input (100) and a large input (10000). Discuss the results

library(microbenchmark)
microbenchmark(play_dice1(100),play_dice2(100),play_dice3(100),play_dice4(100))
microbenchmark(play_dice1(10000),play_dice2(10000),play_dice3(10000),play_dice4(10000))

#For the four versions, built-in R vectorized functions run the fastest which indicates that vectorization greatly improves the speed of operation. However, the loop is really slow.

#e.
#Do you think this is a fair game? Defend your decision with evidence based upon a Monte Carlo simulation.

estimate_mo <- function(reps) {
  x <- sample(1:6, reps, replace = TRUE)
  x135 <- x == 1 | x == 3 | x ==5
  x2 <- x == 2
  x4 <- x == 4
  x6 <- x == 6
  return((-2*sum(x135) + 2*sum(x4) + 4*sum(x6))/reps)
}
estimate_mo(1e8)

#Yes, the game is fair, since the Monte Carlo simulation shows that the expectation is 0.


## Problem 2-Linear Regression

#a.
#The names of the variables in this data are way too long. Rename the columns of the data to more reasonable lengths.

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
#b.
#Restrict the data to cars whose Fuel Type is “Gasoline”.

cars <- cars_data[which(cars_data$fuel_type == "Gasoline"),]

#c.
#Fit a linear regression model predicting MPG on the highway. The predictor of interest is horsepower.
#Briefly discuss the estimated relationship between horsepower and highway MPG. Be precise about the interpretation of the estimated coefficient.

mod1 <- lm(hw_mpg ~ horsepower + torque + height + 
             length + width + as.factor(id_year),data = cars)
mod1

#The estimated coefficient of horsepower is 0.0163556, so the car which has 1 more horsepower will have 0.0163556 increasing highway MPG.

#d.
Refit the model (with lm) and generate an interaction plot, showing how the relationship between horsepower and MPG changes as torque changes. 

mod2 <- lm(hw_mpg ~ horsepower + torque + horsepower:torque + height + 
             length + width + as.factor(id_year),data = cars)
hist(cars$torque)
hist(cars$horsepower)
library(emmeans)
emmip(mod2, torque ~ horsepower, 
      at = list(horsepower = 150:350,torque = c(200,250,300)))
mod2

#e.
#Calculate β from d. manually (without using lm) by first creating a proper design matrix, then using matrix algebra to estimate β. Confirm that you get the same result as lm did prior.

X1 <- rep(1, 4591 )
X <- cbind(X1, cars$horsepower,cars$torque,
           cars$height,cars$length,cars$width,cars$horsepower * cars$torque)
Y <- cbind(cars$hw_mpg)
solve(t(X) %*% X) %*%t(X)%*%Y


