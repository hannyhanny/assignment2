install.packages("faraway")
library(faraway) 
my_data <- data("teengamb")
teengamb
my_data <-teengamb
typeof(my_data)
my_data$sex[which(my_data$sex == 0)] <- "male"
my_data$sex[which(my_data$sex == 1)] <- "female"
factor(my_data$sex)
summary(my_data)
plot(my_data$status, my_data$income, xlab = "status",
     ylab = "income", col = "blue4")
plot(my_data$income, my_data$gamble, xlab = "income",
     ylab = "gamble", col = "purple4")
plot(my_data$verbal, my_data$income, xlab = "verbal",
     ylab = "income", col = "pink3")
plot(my_data$status, my_data$gamble)

length(unique(my_data$verbal))
rank(unique(my_data$verbal))
boxplot(my_data$verbal)
install.packages("ggplot2")
install.packages("dplyr")
my_data_m <- my_data[which(my_data$sex == "male"),]
my_data_f <- my_data[which(my_data$sex == "female"),]
library("ggplot2")
ggplot(my_data,aes("sex", my_data$income))
apply(my_data_m)
summary(my_data_m)
summary(my_data_f)
average_m <- colMeans(my_data_m[,-1])
average_f <- colMeans(my_data_f[,-1])
average_data <-data.frame(average_m, average_f)
res1 <- with(my_data, table(sex,status))
barplot(res1, legend = TRUE)
hist(my_data$status, col=cols,
     main='status', xlab = 'status score', ylab = 'frequency')
cols <- terrain.colors(10) 
hist(my_data$verbal, col=cols,
     main='verbal', xlab = 'verbal score', ylab = 'frequency')
hist(
  my_data$gamble,
  col = cols,
  main = 'gamble',
  xlab = 'expenditure on gambling in pounds per year',
  ylab = 'frequency'
)
plot(my_data$status, my_data$income)
?plot
with(my_data, boxplot(my_data$status ~ my_data$sex, 
                       col="pink",
                       main="status",
                       xlab='sex',
                      ylab="status score"))
with(my_data, boxplot(my_data$income ~ my_data$sex, 
                      col="coral1",
                      main="income",
                      xlab='sex',
                      ylab="in pounds per week"))
with(my_data, boxplot(my_data$verbal ~ my_data$sex, 
                      col="yellow2",
                      main="verbal",
                      xlab='sex',
                      ylab="verbal score"))
with(my_data, boxplot(my_data$gamble ~ my_data$sex, 
                      col="green4",
                      main="gamble",
                      xlab='sex',
                      ylab="gambling in pounds per year"))
with(my_data, boxplot(my_data$status, 
                      col="pink",
                      main="status",
                      xlab='',
                      ylab="status score"))
with(my_data, boxplot(my_data$income, 
                      col="coral1",
                      main="income",
                      xlab='',
                      ylab="in pounds per week"))
4
with(my_data, boxplot(my_data$gamble , 
                      col="green4",
                      main="gamble",
                      xlab='',
                      ylab="gambling in pounds per year"))

