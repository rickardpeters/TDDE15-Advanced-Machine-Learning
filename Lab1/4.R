data("asia")

n = dim(asia)[1]
set.seed(12345)
id = sample(1:n, floor(n*0.8))
data = asia[id,]
test = asia[-id,]



e <- empty.graph(c("A", "T", "L", "B", "E", "X", "D", "S"))

graphviz.plot(e)

##Naive Bayes = all nodes has one edge from the node we want to classify.

##Creating arcs:

arc.set <- matrix(c("S", "A", "S", "T", "S", "L", "S", "B", "S", "E", "S", "X", "S", "D"),
                  ncol=2, byrow=TRUE, dimnames=list(NULL, c("from", "to")))

arcs(e) <- arc.set

##Learning the parameters:
fitted <- bn.fit(e, data, method="bayes")

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
