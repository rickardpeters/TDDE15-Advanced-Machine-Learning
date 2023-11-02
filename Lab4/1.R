
######## 2.1

## 1.

SquaredExpKernel <- function(x1, x2, sigmaf = 1, L = 3) {
  n1 <- length(x1)
  n2 <- length(x2)
  K <- matrix(NA, n1, n2)
  for (i in 1:n2){
    K[,i] <- sigmaf^2 * exp(-0.5 * ( (x1 - x2[i])/L)^2)
  }
  return(K)
}

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

## 2.

sigma.f <- 1
sigma.n <- 0.1
L <- 0.3  
x.seq <- seq(-1,1,0.1)
observation <- c(0.4, 0.719)
postGP <- posteriorGP(observation[1], observation[2], x.seq, sigma.n, SquaredExpKernel, sigma.f, L)

plot(x.seq, postGP$mean, type="b", ylim=c(-2,2))
lines(x.seq, postGP$mean + 1.96*sqrt(postGP$var))
lines(x.seq, postGP$mean - 1.96*sqrt(postGP$var))

## 3.

observation.2 <- data.frame(x=c(0.4,-0.6), y=c(0.719, -0.044))

postGP.2 <- posteriorGP(observation.2$x, observation.2$y, x.seq, sigma.n, SquaredExpKernel, sigma.f, L)

plot(x.seq, postGP.2$mean, type="b", ylim=c(-2,2), col="red")
lines(x.seq, postGP.2$mean + 1.96*sqrt(postGP.2$var))
lines(x.seq, postGP.2$mean - 1.96*sqrt(postGP.2$var))

## 4.

L.3 <- 1

observation.3 <- data.frame(x=c(-1, -0.6, -0.2, 0.4, 0.8), y=c(0.768, -0.044, -0.940, 0.719, -0.664))

postGP.3 <- posteriorGP(observation.3$x, observation.3$y, x.seq, sigma.n, SquaredExpKernel, sigmaf=1, L=L.3)

plot(x.seq, postGP.3$mean, type="b", ylim=c(-2,2), col="blue")
lines(x.seq, postGP.3$mean + 1.96*sqrt(postGP.3$var))
lines(x.seq, postGP.3$mean - 1.96*sqrt(postGP.3$var))



