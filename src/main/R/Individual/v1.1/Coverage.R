library(matrixStats)
source("Inputs.R")

nDays = trueParams["nDays"]
delta = trueParams["delta"]
mu1 = trueParams[["muY"]][1]
mu2 = trueParams[["muY"]][2]
mu3 = trueParams[["muY"]][3]

names = list("delta", "mu[1]", "mu[2]", "mu[3]" )
for ( day in 1:trueParams["nDays"] ){
    for ( i in 1:2 ){
    	deltaStr = paste("delta", toString(i), "[", toString(day), "]", sep = "")
        names = list(names, deltaStr)	
    }
}

coverage = matrix( nrow = 100, ncol = len(names) )
colnames(coverage) = names
print(nDays)

for ( sim in 1:100) {
    dat = readRDS(paste("results/data/samples/bird", toString(sim), ".rds", sep = ""))
    deltas = readRDS(paste("results/data/samples/bird", toString(sim), ".rds", sep = "")
    bounds = colQuantiles(dat, probs = c(0.025, 0.975))

    for ( day in 1:nDays){
        for ( i in 1:2 ){
    	    deltaStr = paste("delta", toString(i), "[", toString(day), "]", sep = "")
	    deltaLow = bounds[deltaStr, 1]
	    deltaHigh = bounds[deltaStr, 2]
	    if( deltaLow < deltas[day,i] && deltaHigh > deltas[day,i]) {
	    	coverage[sim,deltaStr] = 1
            }
	}
    }
       
    if ( bounds["delta",1] < delta && delta < bounds["delta",2] ) { coverage[sim, "delta"] = 1 }
    if ( bounds["mu[1]", 1] < mu1 && mu1 < bounds["mu[1]", 2] ) { coverage[sim, "mu[1]"] = = 1 }
    if ( bounds["mu[2]", 1] < mu2 && mu2 < bounds["mu[2]", 2] ) { coverage[sim, "mu[2]"] =\
 = 1 }
    if ( bounds["mu[3]", 1] < mu3 && mu3 < bounds["mu[3]", 2] ) { coverage[sim, "mu[3]"] =\
 = 1 }



print("Coverages")
print(coverage)
print("SD's")
print(sd)
print(delta_rope)
