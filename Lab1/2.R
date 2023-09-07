## 2.
## package. To load the data, run data("asia"). Learn both the structure and the
## parameters. Use any learning algorithm and settings that you consider appropriate.
## Use the BN learned to classify the remaining 20 % of the Asia dataset in two classes:
##  S = yes and S = no. In other words, compute the posterior probability distribution of S
## for each case and classify it in the most likely class. To do so, you have to use exact
## or approximate inference with the help of the bnlearn and gRain packages, i.e. you
## are not allowed to use functions such as predict. Report the confusion matrix, i.e.
## true/false positives/negatives. Compare your results with those of the true Asia BN,
## which can be obtained by running

library(bnlearn)
library(Rgraphviz)
library(gRain)
library(caret)


##Initializing the data

data("asia")

n = dim(asia)[1]
set.seed(12345)
id = sample(1:n, floor(n*0.8))
data = asia[id,]
test = asia[-id,]


##Learning the structure:

pdag <- hc(data)

##Learning the parameters:
fitted <- bn.fit(pdag, data, method="bayes")

##Compiling conditional probabilities
fitted.grain <- as.grain(fitted)
fitted.grain <- compile(fitted.grain)

##Initializing empyt vector for probabilities
s.scores <- c()

##Using Exact Inference to compute the posterior probability distribution
## and classifying S.

for (i in 1:nrow(test)) {
  
##Extract rows from test data and remove S-column
##to use as our states for the nodes in setEvidence
  
 vector_from_row <- unlist(test[i,])
 vector <- vector_from_row[-which(names(vector_from_row) == "S")]
 object <- setEvidence(fitted.grain, nodes=c("A", "T", "L", "B", "E", "X", "D"),
                       states=as.character(vector))
 query.fit <- querygrain(object, nodes=c("S"))
 
 s.scores <- c(s.scores, ifelse(query.fit[["S"]][["yes"]] >= 0.5, "yes", "no"))
}

##Creating a confusion matrix

confusion.matrix <- confusionMatrix(as.factor(s.scores), reference=as.factor(test$S))




##Again for the true Asia BN:

dag = model2network("[A][S][T|A][L|S][B|S][D|B:E][E|T:L][X|E]")

true.fitted <- bn.fit(dag, data, method="bayes")
true.fitted.grain <- as.grain(true.fitted)
true.fitted.grain <- compile(true.fitted.grain)

true_scores <- c()

for (i in 1:nrow(test)) {
  
  vector_from_row <- unlist(test[i,])
  vector <- vector_from_row[-which(names(vector_from_row) == "S")]
  
  true.object <- setEvidence(true.fitted.grain, nodes=c("A", "T", "L", "B", "E", "X", "D"),
                        states=as.character(vector))
  
  true.query.fit <- querygrain(true.object, nodes=c("S"))
  true_scores <- c(true_scores, ifelse(true.query.fit[["S"]][["yes"]] > 0.5, "yes", "no"))
  
}

true.confusion.matrix <- confusionMatrix(as.factor(true_scores), reference=as.factor(test$S))


################ Assignment 3

##Extracting the Markov Blanket
mb.fitted <- mb(pdag, node="S")

mb.scores <- c()

##Calculating the posterior probability distribution

for (i in 1:nrow(test)) {
  
  vector_from_row <- unlist(test[i,])
  vector <- vector_from_row[c(mb.fitted)]
  
  mb.object <- setEvidence(true.fitted.grain, nodes=c(mb.fitted),
                             states=as.character(vector))
  
  mb.query.fit <- querygrain(mb.object, nodes=c("S"))
  mb.scores <- c(mb.scores, ifelse(mb.query.fit[["S"]][["yes"]] > 0.5, "yes", "no"))
  
}

##Confusion Matrix:

mb.confusion.matrix <- confusionMatrix(as.factor(mb.scores), reference=as.factor(test$S))
