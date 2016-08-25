library(MASS)
library(randomForest)
set.seed(1)

train = sample(c(TRUE, FALSE), dim(Boston)[1]/2, replace=TRUE)

# Like figure 8.10, I am going to use m=p, m = p/2 and m=sqr(p)
# and the number of trees ranges from 1 to 500


x.train = Boston[train,-14]
y.train = Boston$medv[train]
x.test = Boston[!train,-14]
y.test = Boston$medv[!train]

?Boston
p = 13
m1= p
m2 = p/2
m3 = sqrt(p)

rf1 = randomForest(x.train, y.train, x.test, y.test, mtry= m1, ntree=500)
rf2 = randomForest(x.train, y.train, x.test, y.test, mtry= m2, ntree=500)
rf3 = randomForest(x.train, y.train, x.test, y.test, mtry= m3, ntree=500)

plot(1:500, rf1$test$mse, col= "red", type="l", xlab="Number of trees", ylab= "test MSE")
lines(1:500, rf2$test$mse, col= "blue", type="l")
lines(1:500, rf3$test$mse, col="green", type="l")

min(rf1$test$mse)
min(rf2$test$mse)
min(rf3$test$mse)

# expand the problem to boosting algorithm

library(gbm)
set.seed(101)
bst = gbm(medv~., data=Boston[train,], distribution = "gaussian", n.trees=5000,
          interaction.depth=4)
summary(bst)
plot(bst, i="lstat")

yhat = predict(bst, newdata=Boston[!train,], n.tree=5000)
mean((yhat-Boston$medv[!train])^2)

#not superior to random forest for this case