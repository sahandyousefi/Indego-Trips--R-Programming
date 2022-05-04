#R Project
#By Sahand
## Question 1 : Perform polynomial regression to predict wage using age. Use cross-validation to select the optimal degree d for the polynomial. What degree was chosen, and how does this compare to the results of hypothesis testing using ANOVA?
## Make a plot of the resulting polynomial fit to the data.

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


#Question 2: Fit a step function to predict wage using age, and perform cross-validation to choose the optimal number of cuts. Make a plot of the fit obtained.

plot(wage~age, data = Wage, col = "red")
age.range<-range(Wage$age)
age.grid<-seq(from=age.range[1], to = age.range[2])
fit<-lm(wage~poly(age, 3), data = Wage)
preds<-predict(fit, newdata = list(age = age.grid))
lines(age.grid, preds, col = "blue", lwd = 2)


cv.errs<-rep(NA, degree)

for (i in 2:degree) {
  Wage$age.cut<-cut(Wage$age, i)
  fit<-glm(wage ~ age.cut, data = Wage)
  cv.errs[i]<-cv.glm(Wage, fit)$delta[1]
}

plot(2:degree, cv.errs[-1], xlab = 'cut', ylab = 'MSE test', type = 'l')

deg.min<-which.min(cv.errs)

points(deg.min, cv.errs[deg.min], col = 'Darkgrey', cex = 2, pch = 19)


