
######## 2.2
library(kernlab)

data <- read.csv("https://github.com/STIMALiU/AdvMLCourse/raw/master/GaussianProcess/Code/TempTullinge.csv", header=TRUE, sep=";")

time <- seq(1,2186,5)
day <- rep(seq(1,361,5),6)

temps <- data$temp[time]


## 1.

SquaredExpKernel.func <- function(ell=1, sigmaf=1) {
  
  func <- function(x,xp){
  K <- matrix(NA, length(x), length(xp))
  for (i in 1:length(xp)){
    K[,i] <- sigmaf^2 * exp(-0.5 * ( (x - xp[i])/ell)^2)
  }
  return(K)
  }
  class(SquaredExpKernel.func) <- "kernel"
  class(func) <- "kernel"
  return(func)
}

kernel.res <- SquaredExpKernel.func(ell=1, sigmaf=1)
kernel.function <- SquaredExpKernel.func()
kernel.function(1,2)
## 0.6065307

kernelMatrix(kernel = kernel.function, x=c(1,3,4), y = c(2,3,4))
##        [,1]      [,2]      [,3]
## [1,] 0.6065307 0.1353353 0.0111090
## [2,] 0.6065307 1.0000000 0.6065307
## [3,] 0.1353353 0.6065307 1.0000000


## 2.

lm.fit <- lm(temps~time + time^2)
lm.sigma.n <- sd(lm.fit$residuals)


lm.kernel.function <- SquaredExpKernel.func(sigmaf = 20, ell = 0.2)

GP <- gausspr(x=time, y=temps, kernel=lm.kernel.function, var=lm.sigma.n^2, kpar=list(time, temps))
pred <- predict(GP, time)
obs <- data.frame(x=time, y=temps)

plot(time, pred, type="l", pch=14)
points(obs$x, obs$y, col="blue")

## 3.

posteriorGP <- function (X, Y, XStar, sigmaNoise, k, ...) {
  n <- length(X)
  K <-  k(X,X, ...)
  L <- t(chol(K + (sigmaNoise^2) * diag(n)))
  alpha <- solve(t(L), solve(L,Y))
  kStar <- k(X, XStar, ...)
  fMean <- t(kStar) %*% alpha
  v <- solve(L, kStar)
  fVar <- diag(k(XStar, XStar, ...) - t(v) %*% v)
  return(data.frame(mean=fMean, var=fVar))
}

kernel.function.3 <- SquaredExpKernel.func(ell = 0.2, sigmaf = 20)
postGP.3 <- posteriorGP(scale(time), scale(temps), scale(time), lm.sigma.n, k = kernel.function.3)

sd.temp <- sqrt(var(temps))
mean.temp <- mean(temps)

GP.3 <- postGP.3$mean * sd.temp + mean.temp

plot(time, GP.3, type="l", col="blue")

lines(time, GP.3 + 1.96 * sqrt(postGP.3$var), type="l", lty="dotted")
lines(time, GP.3 - 1.96 * sqrt(postGP.3$var), type="l",lty="dotted")
points(obs$x, obs$y)


## 4.

kernel.function.4 <- SquaredExpKernel.func(ell = 0.2, sigmaf = 20)
GP.4 <- gausspr(x = day, y = temps, kernel = kernel.function.4, var = lm.sigma.n^2, kpar = list (time, temps))

pred.4 <- predict(GP.4, day)

plot(time, GP.3, type="l")
lines(time, GP.3 + 1.96 * sqrt(postGP.3$var), type="l", lty="dotted")
lines(time, GP.3 - 1.96 * sqrt(postGP.3$var), type="l",lty="dotted")
points(obs$x, obs$y, col="grey")
lines(time, pred.4, col="blue")


## 5.

ext.kernel.func <- function(sigmaf, ell.1, ell.2, d) {
  
  func <- function(x, y) {
    res <- c()
    
    for (i in 1:length(y)){
      a <- exp( -(2 * sin(pi * abs(x - y[i]) / d)^2) / (ell.1^2))
      b <- exp(-0.5 * ((abs(x - y[i])^2) / ell.2^2))
      
      res[i] <- (sigmaf^2) * a * b
    }
    return(res)
  }
  
  class(func) <- "kernel"
  class(ext.kernel.func) = "kernel"
  return(func)
  
}

kernel.function.5 <- ext.kernel.func(sigmaf = 20, ell.1 = 1, ell.2 = 10, d = (365/sd(time)))

GP.5 <- gausspr(x = time, y = temps, kernel = kernel.function.5, var = lm.sigma.n^2)

pred.5 <- predict(GP.5, time)

plot(time, pred.4, type="l", col="red")
lines(time, GP.3 + 1.96 * sqrt(postGP.3$var), type="l", lty="dotted")
lines(time, GP.3 - 1.96 * sqrt(postGP.3$var), type="l",lty="dotted")
lines(time, pred.5, col="blue")

points(obs$x, obs$y, pch=4, col="grey")

## The periodic kernel can take into account the differences between the years
## and the red line has the same curve for every period, but the blue curve is
## changing over different years, making the periodic kernel more dynamic
## which we can expect as we are adding more tuning parameters.