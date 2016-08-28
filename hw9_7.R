library(ISLR)
library(e1071)
?Auto

#median gas mileage
md.mpg = median(Auto$mpg)
# Note: it's very important to have high as factor or
# I will not be able to plot

Auto$high = as.factor(ifelse(Auto$mpg>md.mpg, 1, 0))

# fit a support vector classifier with various values of cost
set.seed(1)
cv.out = tune(svm, high~., data=Auto, kernel = "polynomial", 
               ranges=list(cost=c(.01, .1, 1, 10), degree=c(1,2,3,4)))
summary(cv.out)

svm.fit = svm(high~., data=Auto, kernel="linear", cost=10)

plot(svm.fit, Auto, mpg ~ displacement)
plot(svm.fit, Auto, mpg ~ cylinders)
plot(svm.fit, Auto, mpg ~ horsepower)

