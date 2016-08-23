rm(list=ls())

library(MASS)
library(splines)
library(boot)
attach(Boston)
fit1 = lm(nox ~ poly(dis,3), data=Boston)

dis.lim = range(dis)
dis.grid = seq(from=dis.lim[1], to=dis.lim[2], by=0.1)
pred = predict(fit1, newdata=list(dis=dis.grid), se=TRUE) 
#without se=TRUE, it does not work for the lines function later. Don't know why
plot(dis, nox, xlim=dis.lim, cex=.5, col="darkgrey")
lines(dis.grid, pred$fit, lwd=2, col="red")

# Plot the polynomial fits for a range of different polynomial degrees(say 1 to 10)
# probably not plotting here
rss = rep(NA, 10)
for (i in 1:10){
  fit = lm(nox~poly(dis,i), data= Boston)
  rss[i] = sum(fit$residuals^2)
}
rss


# perform cross-validation to select the optimal degree for the polynomial
cv.error = rep(NA,15)
for (degree in 1:15){
  glm.fit = glm(nox~poly(dis, degree), data=Boston)
  cv.error[degree] = cv.glm(Boston, glm.fit, K=10)$delta[1]
}
which.min(cv.error)
# so here 3 would be a good polynomial degree for distance variable


#fit a regression spline to predict nox using dis
fit = lm(nox~bs(dis, df=4), data=Boston) 
#here the default knots at uniform quantiles
summary(fit)

plot(dis, nox, col="darkgrey")
pred = predict(fit, newdata= list(dis = dis.grid), se= TRUE)
lines(dis.grid, pred$fit, lwd=2, col="red")

# Now fit a regression spline for a range of degress of freedom
rss2 = rep(NA, 15)
for (degree in 3:15){
  fit = lm(nox~bs(dis, df=degree), data=Boston)
  rss2[degree]= sum(fit$residuals^2)
}
rss2[-c(1,2)]

# perform cross-validation to find the best degree of freedom for a regression spline
cv.error= rep(NA, 20)
for (i in 1:20){
  fit = glm(nox~bs(dis, df=i), data=Boston)
  cv.error[i] = cv.glm(Boston, fit, K=20)$delta[1]
}
cv.error
which.min(cv.error)
# so 10 seems to be the best degree of freedom in this case for regression spline

# Here I expanded the exercise a tbi
# fit a GAM (generalized additive model) to predict nox using dis and zn
gam.fit = lm(nox ~ ns(dis, 3)+ns(zn, 4), data=Boston)
summary(gam.fit)

# here the lm function will not be able to capture the smoothing spline features
install.packages("gam")
library(gam)
gam1 = gam(nox~s(dis,3)+s(zn,3), data=Boston)
gam2 = gam(nox~s(dis,3)+zn,data=Boston)
gam3 = gam(nox~s(dis,3), data=Boston)

anova(gam1, gam2, gam3,test="F")

#here using local regresion fit built in the gam function
gam.lo = gam(nox~s(dis,df=3)+lo(zn), data=Boston)
plot.gam(gam.lo, se=TRUE, col="green")

detach(Boston)
