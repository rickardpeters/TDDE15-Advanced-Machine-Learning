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
