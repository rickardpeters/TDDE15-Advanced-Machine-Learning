
library(HMM)

########## 1.

##Probability that the robot transfers from one state to another, 0,5
##between each state
transProbs <- as.matrix(data.frame(
"0-1" <- c(0.5,0.5,0,0,0,0,0,0,0,0),
"1-2" <- c(0,0.5,0.5,0,0,0,0,0,0,0),
"2-3" <- c(0,0,0.5,0.5,0,0,0,0,0,0),
"3-4" <- c(0,0,0,0.5,0.5,0,0,0,0,0),
"4-5" <- c(0,0,0,0,0.5,0.5,0,0,0,0),
"5-6" <- c(0,0,0,0,0,0.5,0.5,0,0,0),
"6-7" <- c(0,0,0,0,0,0,0.5,0.5,0,0),
"7-8" <- c(0,0,0,0,0,0,0,0.5,0.5,0),
"8-9" <- c(0,0,0,0,0,0,0,0,0.5,0.5),
"9-0" <- c(0.5,0,0,0,0,0,0,0,0,0.5)
))


##Equal probability for [i-2,i+2] (0.2 each)
emissionProbs <- as.matrix(data.frame(
  "0" <- c(0.2,0.2,0.2,0,0,0,0,0,0.2,0.2),
  "1" <- c(0.2,0.2,0.2,0.2,0,0,0,0,0,0.2),
  "2" <- c(0.2,0.2,0.2,0.2,0.2,0,0,0,0,0),
  "3" <- c(0,0.2,0.2,0.2,0.2,0.2,0,0,0,0),
  "4" <- c(0,0,0.2,0.2,0.2,0.2,0.2,0,0,0),
  "5" <- c(0,0,0,0.2,0.2,0.2,0.2,0.2,0,0),
  "6" <- c(0,0,0,0,0.2,0.2,0.2,0.2,0.2,0),
  "7" <- c(0,0,0,0,0,0.2,0.2,0.2,0.2,0.2),
  "8" <- c(0.2,0,0,0,0,0,0.2,0.2,0.2,0.2),
  "9" <- c(0.2,0.2,0,0,0,0,0,0.2,0.2,0.2)
))

states=c("0","1","2","3","4","5","6","7","8","9")
symbols=c("0","1","2","3","4","5","6","7","8","9")

hmm <- initHMM(States = states,
               Symbols = symbols,
               startProbs = c(rep(0.1,10)),
               transProbs = transProbs,
               emissionProbs = emissionProbs)


########## 2.

## Simulating 100 time steps
set.seed(12345)
sim.hmm <- simHMM(hmm, 100)

########## 3.

##Extracting observations
sim <- sim.hmm$observation

##Alpha with forward algorithm, exp because forward() gives log probabilities
alpha <- forward(hmm, sim)
alpha <- exp(alpha)

##Beta with backward algorithm, exp because backward() gives log probabilities
beta <- backward(hmm, sim)
beta <- exp(beta)

##Filtering: alpha(z^t)/sum(over z^t)(alpha(z^t))
filtered <- function(alpha) {
  f.scores <- matrix(nrow=dim(alpha)[1], ncol=dim(alpha)[2])
  for (i in 1:dim(alpha)[2]) {
    f.scores[,i] <- alpha[,i] / sum(alpha[,i])
    
  }
  return(f.scores)
}


##Smoothing: alpha(z^t)*beta(z^t)/sum(over z^t)(alpha(z^t)*beta(z^t))
smoothing <- function(alpha, beta) {
  s.scores <- matrix(nrow=dim(alpha)[1], ncol=dim(alpha)[2])
  for (i in 1:dim(alpha)[2]) {
    s.scores[,i] <- alpha[,i]*beta[,i] / sum(alpha[,i]*beta[,i])
    
  }
  return(s.scores)
}


filtered.hmm <- filtered(alpha)
smoothing.hmm <- smoothing(alpha, beta)
##Using Viterbi for most probable path.
most.prob.path <- viterbi(hmm, sim)

########## 4.

##Take the most likely state for each observation, i.e. max for each column

pred.filtered <- states[apply(filtered.hmm, 2, which.max)]
pred.smoothing <- states[apply(smoothing.hmm, 2, which.max)]

##Confusion matrices

cm.filtered <- table(pred.filtered, sim.hmm$states)
cm.smoothing <- table(pred.smoothing, sim.hmm$states)
cm.viterbi <- table(most.prob.path, sim.hmm$states)

##Function to calculate accuracy. sum(TN + TP) / sum(TN + TP + FN + FP)
sum(cm.filtered)

accuracy <- function (cm) {
  return (sum(diag(cm))/sum(cm))
}

accuracy(cm.filtered)
accuracy(cm.smoothing)
accuracy(cm.viterbi)

########## 5.

## Do again with another seed.
## 23456


########## 6.

## Entropy is a way of saying how uncertain a model
## is of its prediction for a hidden random variable

library(entropy)

e.scores <- c()
for (i in 1:dim(filtered.hmm)[2]) {
  entropy <- entropy.empirical(filtered.hmm[,i])
  e.scores <- c(e.scores, entropy)
}

e.scores
plot(e.scores, type="l", ylab="Entropy", xlab="Time step")

## If entropy is 0, we have made a prediction that we are 100 certain of.

########## 7.

filtered.hmm[,100]

hmm.101 <- as.vector(filtered.hmm[,100] %*% transProbs)
hmm.101



