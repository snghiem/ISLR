set.seed(1)
x = matrix(rnorm(40*2), ncol=2)
y=c(rep(-1,20), rep(1,20))
x[y==1,] = x[y==1,] +1.2

# plot to see if they are linearly separated
plot(x, col=3-y)

dat = data.frame(x, y=as.factor(y)) #here I need y=, without y=, I cannot plot
#install.packages("e1071")
library(e1071)
svm.fit = svm(y~., data = dat, kernel="linear", cost=10, scale=FALSE)
plot(svm.fit,dat)
summary(svm.fit)
svm.fit$index
names(svm.fit)
svm.fit$nclasses
svm.fit$levels
svm.fit$sv

# let's use a smaller value of the cost parameter. Note that
# the cost argument specifies the cost of a violation to the margin.
# When it is small, the margins will be wide and many support vectors are on the
# margin. When it is large, the cost of "violation," thus it's better to have
# narrow margins and so few support vectors on the margin or violating the margin

svm.fit = svm(y~., data=dat, kernel ="linear", cost=0.1, scale=FALSE)
plot(svm.fit, dat)


# I am going to plot my own graph then.
# using make.grid method from ISLR

make.grid = function(x, n=75){
  grid.range = apply(x,2,range)
  x1 = seq(from=grid.range[1,1], to = grid.range[2,1], length=n)
  x2 = seq(from=grid.range[1,2], to = grid.range[2,2], length=n)
  expand.grid(X1=x1, X2=x2)
}

xgrid = make.grid(x)
ygrid = predict(svm.fit, xgrid)
plot(xgrid, col=c("red","blue")[as.numeric(ygrid)], pch=20, cex=.2)
points(x, col=y+3, pch=19)
points(x[svm.fit$index,], pch=5, cex=2)

# So here the number of support vectors increases.
# Now I want to use cross-validation to find the best cost argument for performance
set.seed(123456)
cv.outcome = tune(svm, y~.,data = dat, kernel="linear", ranges = list(cost=c(.001, .01, .1, .25, 1, 5, 10, 100)))
summary(cv.outcome)

#the best peformance is .225, but the best cost is .25

best = cv.outcome$best.model
summary(best)

#now I need a test dataset
xtest = matrix(rnorm(40*2), ncol=2)
ytest = sample(c(-1,1), 40, rep=TRUE)
xtest[ytest==1,]=xtest[ytest==1,]+1
test.dat = data.frame(xtest, y=as.factor(ytest))
ypred = predict(best, test.dat)
table(predict=ypred, truth=test.dat$y)

#error rate
mean(ypred != test.dat$y)

#here is the formula from the ESL book
beta = drop(t(svm.fit$coefs)%*%x[svm.fit$index,])
beta0 = svm.fit$rho
plot(xgrid, col=c("red","blue")[as.numeric(ygrid)], pch=20, cex=.2)
points(x, col=y+3, pch=19)
points(x[svm.fit$index,], pch=5, cex=2)
#draw the hyperplane
abline(beta0/beta[2], -beta[1]/beta[2])
#draw the margins
abline((beta0-1)/beta[2], -beta[1]/beta[2], lty=2)
abline((beta0+1)/beta[2], -beta[1]/beta[2], lty=2)
