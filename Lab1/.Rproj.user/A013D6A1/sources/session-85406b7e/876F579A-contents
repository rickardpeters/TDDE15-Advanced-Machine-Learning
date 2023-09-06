#if (!requireNamespace("BiocManager", quietly = TRUE))
#  install.packages("BiocManager")
#BiocManager::install("RBGL")
#BiocManager::install("Rgraphviz")
#BiocManager::install("gRain")

install.packages("bnlearn")
library(bnlearn)

data("asia")

## HC with default settings:
hc <- hc(asia)
vs.hc <- vstructs(hc)

## HC with Akaike Information Criterion score:

hc.score <- hc(asia, score="aic")
vs.score <- vstructs(hc.score)

graphviz.compare(cpdag(hc), cpdag(hc.score))

vs.score
vs.hc
