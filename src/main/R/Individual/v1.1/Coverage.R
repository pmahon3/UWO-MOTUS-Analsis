library(matrixStats)
source("Inputs.R")

args = commandArgs(trailingOnly=TRUE)
n = strtoi(args[1], base=0L)
days = trueParams[["nDays"]]
delta = trueParams[["delta"]]
mu1 = trueParams[["muY"]][1]
mu2 = trueParams[["muY"]][2]
mu3 = trueParams[["muY"]][3]

penultimateDeltas = matrix(ncol = 6, nrow = n)
colnames(penultimateDeltas) = c("d 2.5%", "d 97.5%", "d2f 2.5%", "d2f 97.5%", "dd 2.5%", "dd 97.5%")

names = list("delta", "mu[1]", "mu[2]", "mu[3]" )
for ( day in 1:days ){
  for ( i in 1:2 ){
    deltaStr = paste("delta", toString(i), "[", toString(day), "]", sep = "")
    names = c(names, deltaStr)	
  }
}

coverage = matrix( ncol = n, nrow = length(names) )
rownames(coverage) = names


for ( sim in 1:n) {
  dat = readRDS(paste("results/samples/bird", toString(sim), ".rds", sep = ""))
  deltas = readRDS(paste("results/data/paramsBird", toString(sim), ".rds", sep = ""))
  bounds = colQuantiles(dat, probs = c(0.025, 0.975))
  
  penultimateDeltas[sim, 1:4] = c(bounds["delta",], bounds[paste("delta2[", toString(days), "]", sep=""),])

  if ( sim == 1){
    boundsAvg = bounds
  }
  else{ 
    boundsAvg = (boundsAvg + bounds)/2
  }
  

  for ( day in 1:days ){
    for ( i in 1:2 ){
      deltaiLow = bounds[ paste("delta", toString(i), "[", toString(day), "]", sep = ""), 1]
      deltaiHigh = bounds[ paste("delta", toString(i), "[", toString(day), "]", sep = ""), 2]
      if( deltaiLow < deltas[day,i] && deltaiHigh > deltas[day,i]) {
       coverage[paste("delta", toString(i), "[", toString(day), "]", sep = ""), sim] = 1
      }
      else{
        coverage[paste("delta", toString(i), "[", toString(day), "]", sep = ""), sim] = 0
      }
      if ( day == days && i == 2){
	newDelta2Low = bounds["delta",1] + deltaiLow
        newDelta2High = bounds["delta",2] + deltaiHigh
	newDeltaLow = newDelta2Low - deltas[day,2]
	newDeltaHigh = newDelta2High - deltas[day,2]
	penultimateDeltas[sim, 5:6] = c(newDeltaLow, newDeltaHigh)
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

print("Mean Coverage:")
cbind(rowMeans(coverage))
print("Bound Averages")
print(boundsAvg)
print("Derived Bound Avrerages for Final Day")
print(colMeans(penultimateDeltas))