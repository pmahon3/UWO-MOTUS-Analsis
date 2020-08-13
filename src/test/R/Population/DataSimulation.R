

dataSimulation <- function(x, CONSTANTS, TRUEPARAMS, saveDat){

  y = array( dim = c(CONSTANTS$nBirds, CONSTANTS$nDays, CONSTANTS$nObservations))
  t = array( dim = c(CONSTANTS$nBirds, CONSTANTS$nDays, CONSTANTS$nObservations))

  ## VECTORS FOR STORING PARAMETERS SIMULATED AT THE INDIVIDUAL LEVEL
  deltaPrimeVec = vector(mode = "double", length = CONSTANTS$nBirds)
  delta1Vec = vector(mode = "double", length = CONSTANTS$nBirds)
  delta2Vec = vector(mode = "double", length = CONSTANTS$nBirds)

  for ( bird in 1:CONSTANTS$nBirds ){

    ## INDIVIDUAL INTERDAY PARAMETERS
    delta1 <- rnorm(1, mean = TRUEPARAMS$muDelta1, sd = TRUEPARAMS$delta1Sd)
    delta2 <- rnorm(1, mean = TRUEPARAMS$muDelta2, sd = TRUEPARAMS$delta2Sd)

    # INDIVIDUAL DELTA PRIME
    deltaPrime = rnorm(1,  mean = TRUEPARAMS$muDeltaPrime, sd = TRUEPARAMS$deltaPrimeSd )

    delta1Vec[bird] = delta1
    delta2Vec[bird] = delta2
    deltaPrimeVec[bird] = deltaPrime

    # INDIVIDUAL MODE SIGNAL STRENGTH PARAMETERS
    muY = vector(mode = "double", length = 3)
    tauY = vector(mode = "double", length = 3)
    for ( j in 1:3 ){
      muY[j] <- rnorm(1, mean = TRUEPARAMS$muMuY[j], sd = TRUEPARAMS$sdMuY[j])
    }

    ## ALL DAYS TO THE PENULTIMATE DAY
    for ( day in 1:CONSTANTS$nDays ){

      ## VECTORS FOR TIME STAMPS
      t1 <- seq( from = delta1-1, to = delta1+1, length.out = CONSTANTS$nObservations / 2)
      t2 <- seq( from = delta2-1, to = delta2+1, length.out = CONSTANTS$nObservations / 2)
      t[bird, day, ] = c(t1,t2)

      ## OBSERVATION SIMULATION
      for ( observation in 1:(CONSTANTS$nObservations)){
        if ( t[bird, day, observation] < delta1 ){
          mu = muY[1]
          sd = TRUEPARAMS$sdY[1]
        }
        else if ( day == CONSTANTS$nDays && t[bird, day, observation] < delta2 + deltaPrime){
          mu = muY[2]
          sd = TRUEPARAMS$sdY[2]
        }
        else if ( t[bird, day, observation] < delta2 ){
          mu = muY[2]
          sd = TRUEPARAMS$sdY[2]
        }
        else{
          mu = muY[3]
          sd = TRUEPARAMS$sdY[3]
        }
        y[bird, day, observation] = rnorm(1, mu, sd)
      }
    }
  }

  simulatedParams = cbind(deltaPrimeVec, delta1Vec, delta2Vec)

  if (saveDat){
    saveRDS(simulatedParams, paste("./results/data/simulatedParams", toString(x), ".rds", sep = ""))
  }
  
  DATA = list(y = y)
  CONSTANTS = c(CONSTANTS, list(t = t))
  return(list(data = DATA, constants = CONSTANTS))
}