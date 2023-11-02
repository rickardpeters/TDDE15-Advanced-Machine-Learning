
######## 2.3
library(kernlab)
library(pracma)
library(knitr)
#library(AtmRay)

data <- read.csv("https://github.com/STIMALiU/AdvMLCourse/raw/master/GaussianProcess/Code/banknoteFraud.csv", header=FALSE, sep=",")
names(data) <- c("varWave","skewWave","kurtWave","entropyWave","fraud")
data[,5] <- as.factor(data[,5])

set.seed(111)
SelectTraining <- sample(1:dim(data)[1], size = 1000, replace = FALSE)

train <- data[SelectTraining,]
test <- data[-SelectTraining,]

## 1.

GP <- gausspr(fraud~varWave+skewWave, data=train)


x1 <- seq(min(train$varWave),max(train$varWave),length=100)
x2 <- seq(min(train$skewWave),max(train$skewWave),length=100)
gridPoints <- meshgrid(x1, x2)
gridPoints <- cbind(c(gridPoints$X), c(gridPoints$Y))
gridPoints <- data.frame(gridPoints)

names(gridPoints) <- names(train)[1:2]
probPreds <- predict(GP, gridPoints, type="probabilities")

contour(x1, x2, matrix(probPreds[,1], 100, byrow=TRUE), 20, xlab="varWave", ylab="skewWave")
points(train[train$fraud=='1', 1], train[train$fraud =='1', 2], col="blue", pch=20)
points(train[train$fraud=='0', 1], train[train$fraud =='0', 2], col="red", pch=20)



pred <- predict(GP, train)
cm.train <- table(pred, train$fraud)

## 2.

test.pred <- predict(GP, test)
cm.test <- table(test.pred, test$fraud)
accuracy <- sum(diag(cm.test / sum(cm.test)))
## 0.9247312

## 3.

GP.3 <- gausspr(fraud~varWave+skewWave+kurtWave+entropyWave, data=train)

pred.3 <- predict(GP.3, test)
cm.3 <- table(pred.3, test$fraud)
accuracy.3 <- sum(diag(cm.3 / sum(cm.3)))
## 0.9946237

## Using only two parameters, we do not have access to the entire data set,
## and all the information it contains. When using all of the parameters
## available, we can make more accurate predictions.
## Both of the models can be considered good considering they both have over
## 90% accuracy, but since the other model has an accuracy that is very close
## to 100%, it will perform considerably better.
