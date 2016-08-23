rm(list=ls())

library(boot)
library(ISLR)
attach(Wage)

# this is a very validation approach, not the cross-validation
# here I don't know why the number of preds are bizzarely different
# from half of a population
degree = 1:10
train = sample(c(TRUE, FALSE), dim(Wage)[1]/2, rep=TRUE)
test = !train
mse = rep(0,10)
for (d in degree) {
  fit = lm(wage~poly(age,d), data=Wage, subset=train)
  pred = predict(fit, newdata=Wage[test,])
  mse[d] = mean((pred - wage[test])^2) 
}
plot(degree, mse)
which.min(mse)
#so age should have 4 degrees of polynomials

library(boot)
set.seed(23)
cv.error = rep(0,10)
for (i in 1:10){
  glm.fit = glm(wage~poly(age,i), data=Wage)
  cv.error[i] = cv.glm(Wage, glm.fit, K=10)$delta[1]
}

plot(1:10,cv.error)
which.min(cv.error)
cv.min = min(cv.error)
cv.sd = sd(cv.error)
abline(h=cv.min + 0.2 * cv.sd, col="red", lty="dashed")
abline(h=cv.min - 0.2 * cv.sd, col="red", lty="dashed")
legend("topright", "0.2-standard deviation lines", lty="dashed", col="red")

# the line shows that d=3 is the smallest degree giving small cross-validation error

#Now we are going to use ANOVA
fit1 = lm(wage~age, data=Wage)
fit2 = lm(wage~poly(age,2), data=Wage)
fit3 = lm(wage~poly(age,3), data=Wage)
fit4 = lm(wage~poly(age,4), data=Wage)
fit5 = lm(wage~poly(age,5), data=Wage)
fit6 = lm(wage~poly(age,6), data=Wage)
fit7 = lm(wage~poly(age,7), data=Wage)
fit8 = lm(wage~poly(age,8), data=Wage)
fit9 = lm(wage~poly(age,9), data=Wage)
fit10 = lm(wage~poly(age,10), data=Wage)
anova(fit1, fit2, fit3, fit4, fit5, fit6, fit7, fit8, fit9, fit10)

# degree of 3 is significant

# Now we plot the resulting polynomial fit to the data
plot(wage~age, data=Wage, col="darkgrey")
agelims = range(age)
age.grid = seq(from=agelims[1], to=agelims[2])
preds = predict(fit, newdata=list(age=age.grid), se=TRUE)
se.bands = cbind(preds$fit + 2*preds$se.fit, preds$fit - 2*preds$se.fit)
lines(age.grid, preds$fit, lwd=2, col="blue")
matlines(age.grid, se.bands, lwd=1, col="blue", lty=3)


# (b) here we need to fit a step function to predict wage using age, and perform
#  cross-validation to choose the optimal number of cuts

cv.error = rep(NA, 20)
# test cross-validation with 20 cuts
for (i in 2:20) { # since the number of segments should be more than 2
  newcut = cut(age,i) 
  fit = glm(wage~newcut, data=Wage)
  # Here I could not include cut(age,i) into glm right away, after ~
  # don't know why 
  # Error in model.frame.default(Terms, newdata, na.action = na.action, xlev = object$xlevels) : 
  #   factor cut(age, i) has new levels (18.9,47], (47,75.1]
  cv.error[i] = cv.glm(Wage, fit, K=10)$delta[2]
}
plot(2:20, cv.error[-1], xlab="Numbers of cuts", ylab="CV error", type="l", pch=20, lwd=2 )

min(cv.error[-1])
which.min(cv.error[-1])

#then I guess 18 would be a good number of cuts.

#Now I need to make a plot of the fit obtained
fit = lm(wage~cut(age,18))
coef(summary(fit))

plot(wage~age, data=Wage, col="darkgrey")
preds = predict(fit, newdata=list(age=age.grid), se=TRUE)
se.bands = cbind(preds$fit + 2*preds$se.fit, preds$fit - 2*preds$se.fit)
lines(age.grid, preds$fit, lwd=2, col="blue")
matlines(age.grid, se.bands, lwd=1, col="blue", lty=3)

detach(Wage)
