set.seed(1)
x =matrix(rnorm(400*2), ncol=2)
x[1:200,]=x[1:200,]+2
x[201:400,] = x[201:400,]-2
y=c(rep(0,250), rep(1,150))
dat = data.frame(x=x, y=as.factor(y))
plot(x, col=y+1)

train = sample(c(TRUE,FALSE),200, rep=T)
svm.fit = svm(y~., data=dat[train,], kernel= "radial", gamma=1, cost=1)
plot(svm.fit, dat[train,])
summary(svm.fit)
###############################
set.seed(1)
cv.out = tune(svm, y~., data=dat[train,], kernel="radial", 
              ranges=list(cost=c(.1,1,10,100,1000), gamma=c(.5,.1,1,2,3,4)))
summary(cv.out)
pred = predict(cv.out$best.model, newx=dat[!train,])
table(truth=y[!train], pred)
# error rate
mean(y[!train]!= pred)

##################
#ROC curves
install.packages("ROCR")
library(ROCR)

svm.out = svm(y~., data=dat[train,], kernel="radial", gamma=.5, 
              cost=.1, decision.values=T)
fitted = attributes(predict(svm.out, dat[train,], decision.values = T))$decision.values
?attributes
# decision.values here are the fitted values

pred = prediction(fitted, dat[train,"y"])
perf = performance(pred,"tpr","fpr")
plot(perf, main="Training Data") # this is just really bad. Why?
# The ROC does not even come over the other half 45 degree line.
