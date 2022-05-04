#Assignment 4
#Team member: Sahand Yousefi- Tinglei Ruan
#Question 1

library(ISLR)
#install.packages('ISLR')
library(boot)

set.seed(33)


degree <- 14

crossv.errors <- rep(NA, degree)


for (i in 1:degree) {
  fit <- glm(wage ~ poly(age, i), data = Wage)
  crossv.errors[i] <- cv.glm(Wage, fit)$delta[1]
}


plot(1:degree, crossv.errors, xlab = 'Degree', ylab = 'Test MSE', type = 'l')

degree.min <- which.min(crossv.errors)

points(degree.min, crossv.errors[degree.min], col = 'red', cex = 2, pch = 19)


#Question 2

plot(wage~age, data = Wage, col = "red")

age.range<-range(Wage$age)

age.grid<-seq(from=age.range[1], to = age.range[2])

fit<-lm(wage~poly(age, 3), data = Wage)

preds<-predict(fit, newdata = list(age = age.grid))

lines(age.grid, preds, col = "blue", lwd = 2)


# Question 3

cv.errs<-rep(NA, degree)

for (i in 2:degree) {
  Wage$age.cut<-cut(Wage$age, i)
  fit<-glm(wage ~ age.cut, data = Wage)
  cv.errs[i]<-cv.glm(Wage, fit)$delta[1]
}

plot(2:degree, cv.errs[-1], xlab = 'cut', ylab = 'MSE test', type = 'l')

deg.min<-which.min(cv.errs)

points(deg.min, cv.errs[deg.min], col = 'Darkgrey', cex = 2, pch = 19)


