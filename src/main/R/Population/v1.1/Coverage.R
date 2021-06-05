library(HDInterval)
## Analyze coverage
NPOPS=25
printPopulationResults = TRUE
# source params
source("Inputs.R")
print(trueParams)
nBirds = trueParams$nBirds


# init coverage results
muDelta.primeCoverage = 0
muMuDeltaCoverage = c(0,0)
muMuMuYCoverage = c(0,0,0)
muDelta.primeCoverage = 0
impliedDeltaPrimeCoverage = 0

for (i in 1:NPOPS){
    
  print(paste("Simulation", toString(i), "Coverage", sep = " "))
  print("------------------------------------------")
    
  # read in data
  samples = data.frame(readRDS(paste("./results/samples/population", toString(i), ".rds", sep = "")))
  simulatedParams = readRDS(paste("./results/data/simulatedParams", toString(i), ".rds", sep = ""))
  
  # init coverage results
  delta.primeCoverage = 0
  muDeltaCoverage = c(0,0)
  deltaCoverage = c(0,0)
  muMuYCoverage = c(0,0,0)
  muYCoverage = c(0,0,0)  
    
  for (bird in 1:nBirds) {
    # delta.prime coverage
    delta.primeStr = paste("delta.prime.", toString(bird), ".", sep="")  
    trueDelta.prime = simulatedParams$deltaPrime[bird]
    delta.primeHdi = hdi(samples[delta.primeStr], 0.95)  
    if (trueDelta.prime > delta.primeHdi[1] && trueDelta.prime < delta.primeHdi[2]){delta.primeCoverage = delta.primeCoverage + 1}
      
    # muDelta coverage
    for (i in 1:2) {
      muDeltaStr = paste("muDelta.", toString(bird), "..", toString(i), ".", sep="")    
      trueMuDelta = simulatedParams$muDelta[bird,i]  
      muDeltaHdi = hdi(samples[muDeltaStr], 0.95)
      if (trueMuDelta > muDeltaHdi[1] && trueMuDelta < muDeltaHdi[2]){muDeltaCoverage[i] = muDeltaCoverage[i] + 1}
    }
    # muMuY coverage
    for (i in 1:3) {
      muMuYStr = paste("muMuY.", toString(bird), "..", toString(i), ".", sep="")
      trueMuMuY = simulatedParams$muMuY[bird,i]
      muMuYHdi = hdi(samples[muMuYStr], 0.95)
      if (trueMuMuY > muMuYHdi[1] && trueMuMuY < muMuYHdi[2]){muMuYCoverage[i] = muMuYCoverage[i] + 1}
    }
  
    for (day in 1:nDays) {
      # delta coverage    
      for (i in 1:2){ 
        deltaStr = paste("delta.", toString(bird), "..", toString(day), "..", toString(i), ".", sep="") 
        trueDelta = simulatedParams$delta[bird,day,i]
        deltaHdi = hdi(samples[deltaStr])
        if (trueDelta > deltaHdi[1] && trueDelta < deltaHdi[2]){deltaCoverage[i] = deltaCoverage[i] + 1}
      }
      # muY coverage
      for (i in 1:3) {  
        muYStr = paste("muY.", toString(bird), "..", toString(day), "..", toString(i), ".", sep="")
        trueMuY = simulatedParams$muY[bird,day,i]
        muYHdi = hdi(samples[muYStr])
        if (trueMuY > muYHdi[1] && trueMuY < muYHdi[2]){muYCoverage[i] = muYCoverage[i] + 1}
      }  
    }
      
    ## Implied delta prime 
    delta2FinalHdi = hdi(samples[paste("delta.", toString(bird), "..", toString(nDays), "..", toString(2), ".", sep="")])
    trueDelta2Final = simulatedParams$delta[bird,nDays,2]
    impliedDeltaPrimeHdi = c(delta2FinalHdi[1] + delta.primeHdi[1] - trueDelta2Final, delta2FinalHdi[2] + delta.primeHdi[2] - trueDelta2Final)
    if (trueDelta.prime > impliedDeltaPrimeHdi[1] && trueDelta.prime < impliedDeltaPrimeHdi[2]){impliedDeltaPrimeCoverage = impliedDeltaPrimeCoverage + 1}
  }
    
  # muDelta.prime coverage
  trueMuDelta.prime = trueParams$muDelta.prime
  muDelta.primeHdi = hdi(samples['muDelta.prime'])  
  print("muDelta.prime:")  
  if (trueMuDelta.prime > muDelta.primeHdi[1] && trueMuDelta.prime < muDelta.primeHdi[2]){
    muDelta.primeCoverage = muDelta.primeCoverage + 1
    print(1)  
  }
  else{print(0)}  
    
  # muMuDelta coverage
  tmp = c(0,0)
  for (i in 1:2) {
    muMuDeltaStr = paste("muMuDelta.", toString(i), ".", sep="")
    muMuDeltaHdi = hdi(samples[muMuDeltaStr], 0.95)
    if (trueParams$muMuDelta[i] > muMuDeltaHdi[1] && trueParams$muMuDelta[i] < muMuDeltaHdi[2]){
      muMuDeltaCoverage[i] = muMuDeltaCoverage[i] + 1
      tmp[i] = 1   
    }
  }
  print("muMuDelta:")
  print(tmp)
      
  # muMuMuY coverage
  tmp = c(0,0,0)
  for (i in 1:3) {
    muMuMuYStr = paste("muMuMuY.", toString(i), ".", sep="")
    muMuMuYHdi = hdi(samples[muMuMuYStr], 0.95)
    if (trueParams$muMuMuY[i] > muMuMuYHdi[1] && trueParams$muMuMuY[i] < muMuMuYHdi[2]){
      muMuMuYCoverage[i] = muMuMuYCoverage[i] + 1
      tmp[i] = 1
    }
  }
  print("muMuMuY:")
  print(tmp)

  # results output
  delta.primeCoverage = delta.primeCoverage/nBirds
  deltaCoverage = deltaCoverage/(nBirds*nDays)
  muDeltaCoverage = muDeltaCoverage/nBirds
  muYCoverage = muYCoverage/(nBirds*nDays)
  muMuYCoverage = muMuYCoverage/nBirds
    

  print("delta.prime:")
  print(delta.primeCoverage)
  print("delta:")
  print(deltaCoverage)
  print("muDelta:")
  print(muDeltaCoverage)
  print("muY:")
  print(muYCoverage)
  print("muMuY:")
  print(muMuYCoverage)  
}

# simulation run results
muMuDeltaCoverage = muMuDeltaCoverage/NPOPS
muMuMuYCoverage = muMuMuYCoverage/NPOPS
muDelta.primeCoverage = muDelta.primeCoverage/NPOPS
impliedDeltaPrimeCoverage = impliedDeltaPrimeCoverage/(NPOPS*nBirds)

print(paste("Overall", "Results", sep = " "))
print("------------------------------------------")
print("muMuDelta:")
print(muMuDeltaCoverage)
print("muMuMuYCoverage")
print(muMuMuYCoverage)
print("muDelta.prime:")
print(muDelta.primeCoverage)
print("implied delta.prime:")
print(impliedDeltaPrimeCoverage)
