library(tree)
library(ISLR)

train = sample(c(TRUE, FALSE), dim(Carseats)[1]/2, rep=TRUE)
test = !train
?Carseats

set.seed(2)

tree.Carseats = tree(Sales~., data=Carseats, subset=train)
summary(tree.Carseats)
plot(tree.Carseats)
text(tree.Carseats, pretty=0)

# test error
pred = predict(tree.Carseats, Carseats[test,])
mean((pred-Carseats$Sales[test])^2)

# Now I use cross-validation to consider whether pruning the true could improve peformance
# prune.tree for regression
# prune.misclass for classification
cv.Carseats = cv.tree(tree.Carseats, FUN = prune.tree)
plot(cv.Carseats$size, cv.Carseats$dev, type='b')
summary(cv.Carseats)
cv.Carseats$dev
?cv.tree
smallest = which.min(cv.Carseats$dev)
smallest
prune.Carseats = prune.tree(tree.Carseats, best=smallest)
plot(prune.Carseats)
text(prune.Carseats)

# Note that if smallest ==1, there will be trouble plotting since there is only a branch
# or simply , no split.

yhat = predict(prune.Carseats, newdata=Carseats[!train,])
mean((yhat-Carseats$Sales[!train])^2)

# there is no improvement in pruning. The MSE increases to 6.1 compared to 4.81 earlier

# now use bagging method
library(randomForest)
set.seed(3)
bag.Carseats = randomForest(Sales~., data= Carseats, subset=train, mtry=10, importance=TRUE)
yhat = predict(bag.Carseats, newdata = Carseats[test,])
mean((yhat-Carseats$Sales[test])^2)

#Yes, significant improvement over performance for MSE test

importance(bag.Carseats)
# this is way too cool

# Ok now I want to use random forest method
set.seed(4)
rf.Carseats = randomForest(Sales~., data=Carseats, subset=train, mtry = 4, importance=TRUE)
yhat = predict(rf.Carseats, newdata = Carseats[test,])
mean((yhat-Carseats$Sales[test])^2)

#does not seem to improve much compared to bagging method
importance(rf.Carseats)
