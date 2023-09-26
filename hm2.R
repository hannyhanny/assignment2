# 500
setwd("/Users/dalaohuhan")
data("uswages")
?lm
lm1 <- lm(wage ~ educ + exper, data = uswages)
summary(lm1)
which.max(residuals(lm1))
a <- c(1,2,9,5,6,4,3,5,8)
which.max(a)
res <- residuals(lm1)

mean(res,trim = 0, na.rm = TRUE)
median(residuals(lm1))
m <- which(uswages$exper < 0)
data_wage <- subset(uswages, uswages$exper >= 0)
lm1 <- lm(wage ~ educ + exper, data = data_wage)
lm1
fitted(lm1)
cor(residuals(lm1),fitted(lm1))
plot(fitted(lm1), residuals(lm1),
     ylab = "residuals", xlab = "fitted values", col="pink",
     main = "residuals against fitted values")
x <- 1:20
y <- x+rnorm(20)
lm2 <- lm(y ~ poly(x, 2, raw = TRUE))
summary(lm2)
plot(x,y, main = "y = b0 + b1x")
lines(x,fitted(lm(y ~ x)), col= "pink3")

plot(x,y, main = "y = b0 + b1x + b2x^2")
lines(x,fitted(lm(y ~ x + I(x^2))), col= "yellow3")

plot(x,y, main = "y = b0 + b1x + b2x^2 + b3x^3 + b4x^4 + b5x^5 + b6x^6")
lines(x,fitted(lm(y ~ x + I(x^2)+I(x^3) + I(x^4) +I(x^5) + I(x^6))),
      col= "blue3")

summary(lm(y ~ x))

lm2 <- lm(y ~ x + I(x^2))
summary(lm(y ~ x + I(x^2) + I(x^3)))
summary(lm(y ~ x + I(x^2) + I(x^3) + I(x^4)))
summary(lm(y ~ x + I(x^2) + I(x^3) + I(x^4) +I(x^5)))
summary(lm(y ~ x + I(x^2) + I(x^3) + I(x^4) +I(x^5) + I(x^6)))
summary(lm2)
xtx <- t(x) %*% x
xtxi <- solve(xtx)
xtxi %*% t(x) %*% y
lm(y ~ x + I(x^2) + I(x^3) + I(x^4) + I(x^5) + I(x^6))
X <- cbind(1, x, x^2, x^3, x^4, x^5, x^6)
beta_direct <- solve(t(X) %*% X) %*% t(X) %*% y
?mean
