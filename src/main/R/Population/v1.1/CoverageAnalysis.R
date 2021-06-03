library(HDInterval)
NPOPS = 6
## Analyze coverage
# Source params
source("Inputs.R")
print(trueParams)
nBirds = trueParams$nBirds

# Init coverage results
muMuDelta1Coverage = 0
muMuDelta2Coverage = 0

for (i in 1:NPOPS){
  
  print(paste("Simulation", toString(i), "Results", sep = " "))
  print("------------------------------------------")
  # read in data
  samples = data.frame(readRDS(paste("./results/samples/population", toString(i), ".rds", sep = "")))
  simulatedParams = readRDS(paste("./results/data/simulatedParams", toString(i), ".rds", sep = ""))
  
  # Print muDelta.prime hdi
  plot(samples$muDelta.prime, type = "l")
  
  # Init coverage results
  muDelta1Coverage = 0
  muDelta2Coverage = 0
  delta1Coverage = 0
  delta2Coverage = 0
  muYCoverage = c(0,0,0)
  
  # muDelta coverage (@bird level)
  for ( bird in 1:nBirds){
    muDelta1str = paste("muDelta.", toString(bird), "..", toString(1), ".", sep="")
    muDelta2str = paste("muDelta.", toString(bird), "..", toString(2), ".", sep="")
    
    trueMuDelta1 = simulatedParams$muDelta[bird, 1]
    trueMuDelta2 = simulatedParams$muDelta[bird, 2]
    muDelta1Hdi = hdi(samples[muDelta1str], 0.95)
    muDelta2Hdi = hdi(samples[muDelta2str], 0.95)
    
    trueMuY = simulatedParams$muY
    
    if (trueMuDelta1 > muDelta1Hdi[1] && trueMuDelta1 < muDelta1Hdi[2]){muDelta1Coverage = muDelta1Coverage + 1}
    if (trueMuDelta2 > muDelta2Hdi[1] && trueMuDelta2 < muDelta2Hdi[2]){muDelta2Coverage = muDelta2Coverage + 1}
    
    # delta coverage (@day level)
    for ( day in 1:nDays){
      delta1str = paste("delta.", toString(bird), "..", toString(day), "..", toString(1), ".", sep="")
      delta2str = paste("delta.", toString(bird), "..", toString(day), "..", toString(2), ".", sep="")
      
      trueDelta1 = simulatedParams$delta[bird,day,1]
      trueDelta2 = simulatedParams$delta[bird,day,2]
      delta1Hdi = hdi(samples[delta1str])
      delta2Hdi = hdi(samples[delta2str])
      
      if (trueDelta1 > delta1Hdi[1] && trueDelta1 < delta1Hdi[2]){delta1Coverage = delta1Coverage + 1}
      if (trueDelta2 > delta2Hdi[1] && trueDelta2 < delta2Hdi[2]){delta2Coverage = delta2Coverage + 1}
    }
  }
  delta1Coverage = delta1Coverage/(nBirds*nDays)
  delta2Coverage = delta2Coverage/(nBirds*nDays)
  muDelta1Coverage = muDelta1Coverage/nBirds
  muDelta2Coverage = muDelta2Coverage/nBirds
  
  print("delta1 Coverage:")
  print(delta1Coverage)
  print("delta2 Coverage:")
  print(delta2Coverage)
  
  print("muDelta1 Coverage:")
  print(muDelta1Coverage)
  print("muDelta2 Coverage:")
  print(muDelta2Coverage)
  
  # muMuDelta coverage (@population level)
  muMuDelta1Hdi = hdi(samples$muMuDelta.1.) 
  muMuDelta2Hdi = hdi(samples$muMuDelta.2.)
  print("muMuDelta1Covered:")
  if (trueParams$muMuDelta1 > muMuDelta1Hdi[1] && trueParams$muMuDelta1 < muMuDelta1Hdi[2]){
    muMuDelta1Coverage = muMuDelta1Coverage + 1
    print(1)
  }
  else{print(0)}
  print("muMuDelta2Covered:")
  if (trueParams$muMuDelta2 > muMuDelta2Hdi[1] && trueParams$muMuDelta2 < muMuDelta2Hdi[2]){
    muMuDelta2Coverage = muMuDelta2Coverage + 1
    print(1)
  }
  else{print(0)}

  
  # print("muMuDelta1Hdi:")
  # print(muMuDelta1Hdi)
  # print("muMuDelta2Hdi:")
  # print(muMuDelta2Hdi)
}


muMuDelta1Coverage = muMuDelta1Coverage/NPOPS
muMuDelta2Coverage = muMuDelta2Coverage/NPOPS

print(paste("Overall", "Results", sep = " "))
print("------------------------------------------")
print("muMuDelta1Coverage:")
print(muMuDelta1Coverage)
print("muMuDelta2Coverage:")
print(muMuDelta1Coverage)