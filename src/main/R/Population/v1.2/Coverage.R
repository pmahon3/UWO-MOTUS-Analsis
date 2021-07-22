library(tibble)
library(dplyr)
library(tidyr)
library(stringr)
library(ggplot2)
library(patchwork)
library(HDInterval)
source("Inputs.R")

#### Simulation Parameters ####

burnin=100
nsamples=300
pop0 = 0

## Generate list of files
dir <- file.path("results","samples")
files <- list.files(dir)
indices <- sort(as.integer(str_extract(files,"\\d+")))
NPOPS <- length(indices)

#### Output Parameters #####
prnt = FALSE
plt = TRUE
save_coverage = TRUE

#### Coverage ####
# containers #
muDelta.primeCoverage = 0
muMuDelta_coverage = matrix(0,nrow = NPOPS, ncol = 2)
delta_coverage = list()
delta.prime_coverage = matrix(nrow=NPOPS,ncol=nBirds)

for (pop in indices){

  # read data
  samples = data.frame(readRDS(paste("./results/samples/population", toString(pop + pop0), ".rds", sep = "")))
  simulatedParams = readRDS(paste("./results/data/simulatedParams", toString(pop + pop0), ".rds", sep = ""))

  # containers
  delta.primeCoverage = vector(mode="numeric",length=nBirds)
  muDeltaCoverage = c(0,0)
  delta_coverage[[pop]]= list(matrix(nrow=nBirds,ncol=nDays), matrix(nrow=nBirds,ncol=nDays))

  for (bird in 1:nBirds) {
    # delta.prime coverage
    delta.primeStr = paste("delta.prime.", toString(bird), ".", sep="")
    trueDelta.prime = simulatedParams$deltaPrime[bird]
    delta.primeHdi = hdi(samples[[delta.primeStr]][burnin:length(samples[[delta.primeStr]])], 0.95)
    if (trueDelta.prime > delta.primeHdi[1] && trueDelta.prime < delta.primeHdi[2]){delta.prime_coverage[pop,bird] = 1}
    else{delta.prime_coverage[pop,bird]=0}

    # muDelta
    for (i in 1:2) {
      muDeltaStr = paste("muDelta.", toString(bird), "..", toString(i), ".", sep="")
      trueMuDelta = simulatedParams$muDelta[bird,i]
      muDeltaHdi = hdi(samples[[muDeltaStr]][burnin:length(samples[[muDeltaStr]])], 0.95)
      if (trueMuDelta > muDeltaHdi[1] && trueMuDelta < muDeltaHdi[2]){muDeltaCoverage[i] = muDeltaCoverage[i] + 1}
    }
    # delta to final day
    for (day in 1:(nDays-1)) {
      for (i in 1:2){
        deltaStr = paste("delta.", toString(bird), "..", toString(day), "..", toString(i), ".", sep="")
        trueDelta = simulatedParams$delta[bird,day,i]
        deltaHdi = hdi(samples[[deltaStr]][burnin:length(samples[[deltaStr]])])
        if (trueDelta > deltaHdi[1] && trueDelta < deltaHdi[2]){delta_coverage[[pop]][[i]][bird,day] = 1}
        else{delta_coverage[[pop]][[i]][bird,day] = 0}

        # if (i==2 && day==nDays){
        #   par(mfrow=c(2,1))
        #   plot(samples[[deltaStr]][burnin:length(samples[[deltaStr]])], type = "l", main = paste("Bird", toString(bird), "Day", toString(day), 'Change Point', toString(i)))
        #   abline(a=trueDelta, b=0, lty = 3, col = 'red')
        #   abline(a=deltaHdi[1], b = 0, lty = 2, col = 'blue')
        #   text(nsamples-burnin, deltaHdi[1], labels=toString(round(deltaHdi[1],2)), cex=0.7, pos=1, offset=0.2)
        #   text(nsamples-burnin, deltaHdi[2], labels=toString(round(deltaHdi[2],2)), cex=0.7, pos=3, offset=0.2)
        #   mtext(side=4, at=trueDelta, text=toString(round(trueDelta,2)), cex=0.7, col='red')
        #   abline(a=deltaHdi[2], b = 0, lty = 2, col = 'blue')
        #
        #   plot(samples[[delta.primeStr]][burnin:length(samples[[delta.primeStr]])], type = "l", main = paste("Bird", toString(bird), "Day", toString(day), 'Delta Prime'))
        #   abline(a=trueDelta.prime, b=0, lty = 3, col = 'red')
        #   abline(a=delta.primeHdi[1], b = 0, lty = 2, col = 'blue')
        #   text(nsamples-burnin, delta.primeHdi[1], labels=toString(round(delta.primeHdi[1],2)), cex=0.7, pos=1, offset=0.2)
        #   text(nsamples-burnin, delta.primeHdi[2], labels=toString(round(delta.primeHdi[2],2)), cex=0.7, pos=3, offset=0.2)
        #   mtext(side=4, at=trueDelta.prime, text=toString(round(trueDelta.prime,2)), cex=0.7, col='red')
        #   abline(a=delta.primeHdi[2], b = 0, lty = 2, col = 'blue')
        #
        #   print(trueDelta.prime)
        #   print(delta.primeHdi)
        # }
        # else{
        #   print(paste('delta', toString(i), toString(day),toString(bird)))
        #   plot(samples[[deltaStr]], type = "l", main = paste("Bird", toString(bird), "Day", toString(day), 'Change Point', toString(i)))
        #   abline(a=trueDelta, b=0, lty = 3, col = 'red')
        #   abline(a=deltaHdi[1], b = 0, lty = 2, col = 'blue')
        #   text(nsamples, deltaHdi[1], labels=toString(round(deltaHdi[1],2)), cex=0.7, pos=1, offset=0.2)
        #   text(nsamples, deltaHdi[2], labels=toString(round(deltaHdi[2],2)), cex=0.7, pos=3, offset=0.2)
        #   mtext(side=4, at=trueDelta, text=toString(round(trueDelta,2)), cex=0.7, col='red')
        #   abline(a=deltaHdi[2], b = 0, lty = 2, col = 'blue')
        # }
      }
    }
    # delta on final day
    # delta1
    deltaStr = paste("delta.", toString(bird), "..", toString(nDays), "..", toString(1), ".", sep="")
    trueDelta = simulatedParams$delta[bird,nDays,1]
    deltaHdi = hdi(samples[[deltaStr]][burnin:length(samples[[deltaStr]])])
    if (trueDelta > deltaHdi[1] && trueDelta < deltaHdi[2]){delta_coverage[[pop]][[1]][bird,nDays] = 1}
    else{delta_coverage[[pop]][[1]][bird,nDays] = 0}

    #delta2
    deltaStr = paste("delta.", toString(bird), "..", toString(nDays), "..", toString(2), ".", sep="")
    trueDelta = simulatedParams$delta[bird,nDays,2]
    delta_minus_delta.primeHdi = hdi(samples[[deltaStr]][burnin:length(samples[[deltaStr]])] - samples[[delta.primeStr]][burnin:length(samples[[delta.primeStr]])])
    if (trueDelta > delta_minus_delta.primeHdi[1] && trueDelta < delta_minus_delta.primeHdi[2]){delta_coverage[[pop]][[2]][bird,nDays] = 1}
    else{delta_coverage[[pop]][[2]][bird,nDays] = 0}
  }

  # muDelta.prime
  trueMuDelta.prime = trueParams$muDelta.prime
  muDelta.primeHdi = hdi(samples[['muDelta.prime']][burnin:length(samples[['muDelta.prime']])])
  if (trueMuDelta.prime > muDelta.primeHdi[1] && trueMuDelta.prime < muDelta.primeHdi[2]){
    muDelta.primeCoverage = muDelta.primeCoverage + 1
  }

  # muMuDelta
  for (i in 1:2) {
    muMuDeltaStr = paste("muMuDelta.", toString(i), ".", sep="")
    muMuDeltaHdi = hdi(samples[[muMuDeltaStr]][burnin:length(samples[[muMuDeltaStr]])], 0.95)
    muMuDelta_coverage[pop, i] = min(c(trueParams$muMuDelta[i] - muMuDeltaHdi[1],
                                       muMuDeltaHdi[2] - trueParams$muMuDelta[i]))
  }

  delta_coverage[[pop]][[1]] = cbind(delta_coverage[[pop]][[1]], rowMeans(delta_coverage[[pop]][[1]]))
  delta_coverage[[pop]][[2]] = cbind(delta_coverage[[pop]][[2]], rowMeans(delta_coverage[[pop]][[2]]))
  delta_coverage[[pop]][[1]] = rbind(delta_coverage[[pop]][[1]], colMeans(delta_coverage[[pop]][[1]]))
  delta_coverage[[pop]][[2]] = rbind(delta_coverage[[pop]][[2]], colMeans(delta_coverage[[pop]][[2]]))
  muDeltaCoverage = muDeltaCoverage/nBirds

  if (save_coverage){
    write.csv(delta_coverage[[pop]][[1]], file=paste("./results/coverage/delta/population_", toString(pop + pop0), "_delta_", toString(1), "_coverage.csv", sep=""))
    write.csv(delta_coverage[[pop]][[2]], file=paste("./results/coverage/delta/population_", toString(pop + pop0), "_delta_", toString(2), "_coverage.csv", sep=""))
  }

  if (prnt) {
    print(paste("Simulation", toString(pop), "Coverage", sep = " "))
    print("------------------------------------------")
    print(muDelta.primeHdi)
    print("delta.prime:")
    print(delta.primeCoverage)
    print("delta:")
    print(delta_coverage)
    print("muDelta:")
    print(muDeltaCoverage)
  }
  if (plt){
    par(mfrow=c(2,1))
    plot(samples$muDelta.prime, type = "l", main = paste("Population", toString(pop)))
    abline(a=trueParams$muDelta.prime, b=0, lty = 3, col = 'red')
    abline(a=muDelta.primeHdi[1], b = 0, lty = 2, col = 'blue')
    text(nsamples, muDelta.primeHdi[1], labels=toString(round(muDelta.primeHdi[1],2)), cex=0.7, pos=1, offset=0.2)
    text(nsamples, muDelta.primeHdi[2], labels=toString(round(muDelta.primeHdi[2],2)), cex=0.7, pos=3, offset=0.2)
    mtext(side=4, at=trueParams$muDelta.prime, text=toString(round(trueParams$muDelta.prime,2)), cex=0.7, col='red')
    abline(a=muDelta.primeHdi[2], b = 0, lty = 2, col = 'blue')
    plot(samples$muMuDelta.2., type = "l")
    abline(a=trueParams$muMuDelta[2], b=0, lty = 3, col = 'red')
    text(nsamples, muMuDeltaHdi[1], labels=toString(round(muMuDeltaHdi[1],2)), cex=0.7, pos=1, offset=0.2)
    text(nsamples, muMuDeltaHdi[2], labels=toString(round(muMuDeltaHdi[2],2)), cex=0.7, pos=3, offset=0.2)
    mtext(side=4, at=trueParams$muMuDelta[2], text=toString(round(trueParams$muMuDelta[2],2)), cex=0.7, col='red')
    abline(a=muMuDeltaHdi[1], b = 0, lty = 2, col = 'blue')
    abline(a=muMuDeltaHdi[2], b = 0, lty = 2, col = 'blue')
  }
}

delta.prime_coverage = cbind(delta.prime_coverage, rowMeans(delta.prime_coverage))

if (save_coverage){
  write.csv(delta.prime_coverage, file="./results/coverage/delta.prime_coverage.csv")
}


# simulation run results
muMuDeltaCoverage = apply(muMuDelta_coverage > 0, 2, mean)

print(paste("Overall", "Results", sep = " "))
print("------------------------------------------")
print("muMuDelta:")
print(muMuDeltaCoverage)
print("muDelta.prime:")
print(muDelta.primeCoverage)
