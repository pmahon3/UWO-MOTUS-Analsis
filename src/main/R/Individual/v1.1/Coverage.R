library(matrixStats)
source("Inputs.R")

days = trueParams[["nDays"]]
print(days)
delta = trueParams[["delta"]]
mu1 = trueParams[["muY"]][1]
mu2 = trueParams[["muY"]][2]
mu3 = trueParams[["muY"]][3]

names = list("delta", "mu[1]", "mu[2]", "mu[3]" )
for ( day in 1:days ){
  for ( i in 1:2 ){
    deltaStr = paste("delta", toString(i), "[", toString(day), "]", sep = "")
    names = c(names, deltaStr)	
  }
}

coverage = matrix( ncol = 100, nrow = length(names) )
rownames(coverage) = names


for ( sim in 1:100) {
  dat = readRDS(paste("results/samples/bird", toString(sim), ".rds", sep = ""))
  deltas = readRDS(paste("results/data/paramsBird", toString(sim), ".rds", sep = ""))
  bounds = colQuantiles(dat, probs = c(0.025, 0.975))
  print(bounds)
  if ( sim == 1){
    boundsAvg = bounds
  }
  else{ 
    boundsAvg = (boundsAvg + bounds)/2
  }
  

  for ( day in 1:days ){
    for ( i in 1:2 ){
	    deltaLow = bounds[ paste("delta", toString(i), "[", toString(day), "]", sep = ""), 1]
	    deltaHigh = bounds[ paste("delta", toString(i), "[", toString(day), "]", sep = ""), 2]
	    if( deltaLow < deltas[day,i] && deltaHigh > deltas[day,i]) {
	    	coverage[paste("delta", toString(i), "[", toString(day), "]", sep = ""), sim] = 1
	    }
	    else{
		coverage[paste("delta", toString(i), "[", toString(day), "]", sep = ""), sim] = 0
	    }
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
cbind(rowMeans(coverage))
