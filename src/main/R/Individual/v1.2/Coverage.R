library(matrixStats)
source("Inputs.R")
options(scipen=999)
args = commandArgs(trailingOnly=TRUE)

n = strtoi(args[1], base = 0L)
days = trueParams[["nDays"]]
delta = trueParams[["delta"]]
delta1 = trueParams[["muDelta1"]]
delta2 = trueParams[["muDelta2"]]
deltas = c(delta1, delta2)
mu1 = trueParams[["muY"]][1]
mu2 = trueParams[["muY"]][2]
mu3 = trueParams[["muY"]][3]

names = list("delta", "delta1", "delta2", "mu[1]", "mu[2]", "mu[3]" )

coverage = matrix( ncol = 100, nrow = length(names) )
rownames(coverage) = names


for ( sim in 1:n) {
  dat = readRDS(paste("results/samples/bird", toString(sim), ".rds", sep = ""))[200:500,]
  bounds = colQuantiles(dat, probs = c(0.025, 0.975))
  if ( sim == 1){
    boundsTot = bounds
  }
  else{ 
    boundsTot = boundsTot + bounds
  }
   

  
  for ( i in 1:2 ){
    deltaLow = bounds[ paste("delta", toString(i), sep = ""), 1]
    deltaHigh = bounds[ paste("delta", toString(i), sep = ""), 2]
    if( deltaLow < deltas[i] && deltaHigh > deltas[i]) {
      coverage[paste("delta", toString(i), sep = ""), sim] = 1
    }
    else{
      coverage[paste("delta", toString(i),  sep = ""), sim] = 0
    }
  }
  
  if ( bounds["delta",1] < delta && delta < bounds["delta",2] ) { coverage["delta", sim] = 1 }
  else { coverage["delta", sim] = 0 }
  if ( bounds["mu[1]", 1] < mu1 && mu1 < bounds["mu[1]", 2] ) { coverage["mu[1]", sim] = 1 }
  else { coverage["mu[1]", sim] = 0 }
  if ( bounds["mu[2]", 1] < mu2 && mu2 < bounds["mu[2]", 2] ) { coverage["mu[2]", sim] = 1 }
  else { coverage["mu[2]", sim] = 0 }
  if ( bounds["mu[3]", 1] < mu3 && mu3 < bounds["mu[3]", 2] ) { coverage["mu[3]",sim] = 1 }
  else { coverage["mu[3]", sim] = 0 }
}
print(boundsTot/n)