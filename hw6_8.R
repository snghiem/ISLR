set.seed(1)
x = rnorm(100)

set.seed(2)
e = rnorm(100)

b0 = 3
b1 = -4.5
b2 = .05
b3 = 12


y = b0 + b1*x +b2*x^2 + b3*x^3 + e


dat = data.frame(y,x)

require(leaps)
regfit.full=regsubsets(y~poly(x,10), data=dat, nvmax =10)
reg.summary = summary(regfit.full)
names(reg.summary)
par(mfrow=c(2,2))
plot(reg.summary$rss, xlab="No. of variables", ylab="RSS")

plot(reg.summary$adjr2, xlab="No. of variables", ylab="Adjusted Rsq")
which.max(reg.summary$adjr2)
points(7, reg.summary$adjr2[7], col="red", cex=2, pch=20)

plot(reg.summary$cp, xlab="No. of variables", ylab="Cp")
which.min(reg.summary$cp)
points(6, reg.summary$cp[6], col="red", cex=2, pch=20)

plot(reg.summary$bic, xlab="No. of variables", ylab="BIC")
which.min(reg.summary$bic)
points(4, reg.summary$bic[4], col="red", cex=2, pch=20, type="l")

par(mfrow=c(1,1))
plot(regfit.full, scale="r2")
plot(regfit.full, scale="adjr2")
plot(regfit.full, scale="Cp")
plot(regfit.full, scale="bic")
coef(regfit.full,4)

# doing forward and backward stepwise selection
regfit.fwd = regsubsets(y~poly(x,10), data=dat, nvmax=10, method="forward")
summary(regfit.fwd)

regfit.bwd = regsubsets(y~poly(x,10), data=dat, nvmax=10, method="backward")
summary(regfit.bwd)


# creat x and y
x.mat = as.matrix(poly(x,10))

library(glmnet)
grid = 10 ^seq(10, -2, length=100)
train=sample(length(y), length(y)/2)
train
test = -train
y.test = y[test]


# Use cross-validation to select the optimal value of lambda for lasso fit
lasso.mod  =glmnet(x.mat[train,], y[train], alpha=1, lambda=grid)
plot(lasso.mod)

cv.out = cv.glmnet(x.mat[train,], y[train], alpha=1)
plot(cv.out)
best.lambda = cv.out$lambda.min
best.lambda

lasso.pred = predict(lasso.mod, s=best.lambda, newx=x[test,])
mean((lasso.pred - y.test)^2)

out = glmnet(x.mat,y, alpha=1, lambda =grid)
dim(coef(out))
lasso.coef = predict(out, type="coefficients", s=best.lambda)[1:11,]
lasso.coef

# well it does not predict well. What if I do the fit lasso on the whole dataset
# and then try to predict on the whole dataset again
# there is a solution on this approach online

# Now generate a different response of y
b7 = 3.4
y = b0+ b7*x^7 +e

#Now perform best subset selection and the lasso. Discuss the results
regfit.full = regsubsets(y~poly(x,10), data=dat, nvmax=10)
reg.summary = summary(regfit.full)
plot(reg.summary$cp, xlab="No. of variables", ylab="Cp")
which.min(reg.summary$cp)
points(6, reg.summary$cp[6], col="red", cex=2, pch=20)

x.mat = model.matrix(y~poly(x,10), data=dat)[,-1]
lasso.mod =glmnet(x.mat,y,alpha=1)
plot(lasso.mod)
cv.out = cv.glmnet(x.mat, y, alpha=1)
best.lambda = cv.out$lambda.min
best.lambda
