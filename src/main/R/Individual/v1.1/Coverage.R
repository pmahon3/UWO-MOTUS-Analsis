## Load packages
library(matrixStats)

## Source inputs
source("Inputs.R")

if(!interactive){
  ## Read command line arguments
  args = commandArgs(trailingOnly=TRUE)
  n = strtoi(args[1], base=0L)
}
else{
  ## Set arguments manually
  n = 100
}

## Set true parameters
days = trueParams[["nDays"]]
delta = trueParams[["delta"]]
mu1 = trueParams[["muY"]][1]
mu2 = trueParams[["muY"]][2]
mu3 = trueParams[["muY"]][3]

## Create container to hold stats for select parameters
penultimateDeltas = matrix(ncol = 6, nrow = n)
colnames(penultimateDeltas) = c("d 2.5%", "d 97.5%", "d2f 2.5%", "d2f 97.5%", "dd 2.5%", "dd 97.5%")

names = list("delta", "mu[1]", "mu[2]", "mu[3]" )
for ( day in 1:days ){
  for ( i in 1:2 ){
    deltaStr = paste("delta", i, "[", day, "]", sep = "")
    names = c(names, deltaStr)	
  }
}

coverage = matrix( ncol = n, nrow = length(names) )
rownames(coverage) = names

## Initialize progressbar
if(interactive())
  pb <- txtProgressBar(0,n,style = 3)
  
## Looping over each bird
for ( sim in 1:n) {
  ## Update progress bar
  setTxtProgressBar(pb, sim)
  
  ## Load MCMC output for bird i
  dat = readRDS(paste("results/samples/bird", sim, ".rds", sep = ""))

  ## Read delta parameters for bird i
  deltas = readRDS(paste("results/data/paramsBird", sim, ".rds", sep = ""))

  ## Estimate bounds of 95% credible intervals for each parameter
  bounds = colQuantiles(dat, probs = c(0.025, 0.975))

  ## Extract statistics for select parameters
  penultimateDeltas[sim, 1:4] = c(bounds["delta",], bounds[paste("delta2[", days, "]", sep=""),])

  ## Compute average bounds for each parameter
  if ( sim == 1){
    boundsAvg = bounds/n
  }
  else{ 
    boundsAvg = boundsAvg/n + bounds/n
  }
  
  ## Loop over days and changepoints
  for ( day in 1:days ){
    for ( i in 1:2 ){

      ## Extract bounds for specified changepoint
      deltaiLow = bounds[ paste("delta", i, "[", day, "]", sep = ""), 1]
      deltaiHigh = bounds[ paste("delta", i, "[", day, "]", sep = ""), 2]

      ## Identify if truth is covered or not
      coverage[paste("delta", i, "[", day, "]", sep = ""), sim] = ( deltaiLow < deltas[day,i] && deltaiHigh > deltas[day,i]) 

      ## Computing derived values (??)
      if ( day == days && i == 2){
	newDelta2Low = bounds["delta",1] + deltaiLow
        newDelta2High = bounds["delta",2] + deltaiHigh
	newDeltaLow = newDelta2Low - deltas[day,2]
	newDeltaHigh = newDelta2High - deltas[day,2]
	penultimateDeltas[sim, 5:6] = c(newDeltaLow, newDeltaHigh)
      }
    }
  }

  ## Compute coverage for remaining parameters
  coverage["delta", sim] = ( bounds["delta",1] < delta && delta < bounds["delta",2] )
  coverage["mu[1]", sim] = ( bounds["mu[1]", 1] < mu1 && mu1 < bounds["mu[1]", 2] ) 
  coverage["mu[2]", sim] = ( bounds["mu[2]", 1] < mu2 && mu2 < bounds["mu[2]", 2] )
  coverage["mu[3]",sim] = ( bounds["mu[3]", 1] < mu3 && mu3 < bounds["mu[3]", 2] )
}

## Print output
print("Mean Coverage:")
cbind(rowMeans(coverage))
print("Bound Averages")
print(boundsAvg)
print("Derived Bound Averages for Final Day")
print(colMeans(penultimateDeltas))
