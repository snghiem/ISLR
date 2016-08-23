rm(list=ls())
library(ISLR)
?College

# split the data set into a training set and a test set
set.seed(101)
train = sample(c(TRUE,FALSE), nrow(College)/2, rep=TRUE)
test = !train
lm.fit = lm(Apps~., data=College, subset=train)
lm.pred = predict(lm.fit, College) #here the argument is newdata, not data, so 
# just don't put in 'data' or 'newdata' at all, just put the name of the data
# since sometimes it can be very easy to forget
mean((lm.pred-College$Apps)[test]^2)
?predict.lm

#Fit a ridge regression on the training set
x = model.matrix(Apps~., College)[,-1]
y = College$Apps

library(glmnet)
ridge.mod = glmnet(x[train,], y[train], alpha=0)
cv.out = cv.glmnet(x[train,], y[train], alpha=0)
plot(cv.out)
best.lambda = cv.out$lambda.min
best.lambda
ridge.pred = predict(ridge.mod, s=best.lambda, newx=x[test,])
mean((ridge.pred-y[test])^2)

#Fit a lasso regression on the training set
lasso.mod = glmnet(x[train,], y[train], alpha=1)
cv.out = cv.glmnet(x[train,], y[train], alpha=1)
plot(cv.out)
best.lambda = cv.out$lambda.min
best.lambda
lasso.pred = predict(lasso.mod, s=best.lambda, newx=x[test,])
mean((lasso.pred-y[test])^2)

# fit PCR
install.packages("pls")
library(pls)
set.seed(3)
pcr.fit = pcr(Apps~., data=College, subset=train, scale=TRUE, validation="CV")
summary(pcr.fit)

validationplot(pcr.fit, val.type = "MSEP")
names(pcr.fit$validation)
pcr.fit$validation["adj"] 
# a matrix of adjustment values for calculating bias corrected MSEP. MSEP uses this
#I approximate that the lowest (approx.) cross-validation error occurs when M=12

pcr.pred =predict(pcr.fit, x[test,], ncomp=15)
mean((pcr.pred-y[test])^2)

#now we fit pcr on the full data set, using M=15
pcr.fit = pcr(Apps~., data=College, scale=TRUE, ncomp=15)
summary(pcr.fit)

# fit PLS
set.seed(100123)
pls.fit = plsr(Apps~., data=College, subset=train, scale=TRUE, validation="CV")
summary(pls.fit)

validationplot(pls.fit, val.type = "MSEP")
names(pls.fit$validation)
pls.fit$validation["adj"] 
# a matrix of adjustment values for calculating bias corrected MSEP. MSEP uses this
#I approximate that the lowest (approx.) cross-validation error occurs when M=7

pcr.pred =predict(pcr.fit, x[test,], ncomp=7)
mean((pcr.pred-y[test])^2)

#now we fit pcr on the full data set, using M=7
pcr.fit = pcr(Apps~., data=College, scale=TRUE, ncomp=7)
summary(pcr.fit)

