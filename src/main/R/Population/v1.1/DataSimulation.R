

dataSimulation <- function(x, CONSTANTS, TRUEPARAMS, saveDat){

  ## Initialize storage matrices and vectors
  y = array( dim = c(CONSTANTS$nBirds, CONSTANTS$nDays, CONSTANTS$nObservations))
  t = array( dim = c(CONSTANTS$nBirds, CONSTANTS$nDays, CONSTANTS$nObservations))
  deltaPrimeVec = vector(mode = "double", length = CONSTANTS$nBirds)
  delta1Vec = vector(mode = "double", length = CONSTANTS$nBirds)
  delta2Vec = vector(mode = "double", length = CONSTANTS$nBirds)

  # Loop over birds
  for ( bird in 1:CONSTANTS$nBirds ){

    # Simulate mean change points, delta change
    delta1 <- rnorm(1, mean = TRUEPARAMS$muMuDelta1, sd = TRUEPARAMS$sigmaMuDelta1)
    delta2 <- rnorm(1, mean = TRUEPARAMS$muMuDelta2, sd = TRUEPARAMS$sigmaMuDelta2)
    deltaPrime = rnorm(1,  mean = TRUEPARAMS$muDelta.prime, sd = TRUEPARAMS$sigmaDelta.prime )

    delta1Vec[bird] = delta1
    delta2Vec[bird] = delta2
    deltaPrimeVec[bird] = deltaPrime

    # Individual signal strength means
    muY = vector(mode = "double", length = 3)
    sdY = vector(mode = "double", length = 3)
    for ( j in 1:3 ){
      muY[j] <- rnorm(1, mean = TRUEPARAMS$muMuY[j], sd = TRUEPARAMS$sigmaMuY[j])
      sdY[j] <- rgamma(1, shape = TRUEPARAMS$shapeSdY[j], scale = TRUEPARAMS$scaleSdY[j])
    }
    
    # Individual signal strength standard deviations

    ## All days to the penultimate day
    for ( day in 1:CONSTANTS$nDays ){

      ## Vecotrs for the time stamps
      t1 <- seq( from = delta1-1, to = delta1+1, length.out = CONSTANTS$nObservations / 2)
      t2 <- seq( from = delta2-1, to = delta2+1, length.out = CONSTANTS$nObservations / 2)
      t[bird, day, ] = c(t1,t2)

      ## Observation mode logic
      for ( observation in 1:(CONSTANTS$nObservations)){
        if ( t[bird, day, observation] < delta1 ){
          mu = muY[1]
          sd = sdY[1]
        }
        else if ( day == CONSTANTS$nDays && t[bird, day, observation] < delta2 + deltaPrime){
          mu = muY[2]
          sd = sdY[2]
        }
        else if ( t[bird, day, observation] < delta2 ){
          mu = muY[2]
          sd = sdY[2]
        }
        else{
          mu = muY[3]
          sd = sdY[3]
        }
        y[bird, day, observation] = rnorm(1, mu, sd)
      }
    }
  }

  simulatedParams = cbind(deltaPrimeVec, delta1Vec, delta2Vec)

  if (saveDat){
    file =  paste("./results/data/simulatedParams", toString(x), ".rds", sep = "")
    print(paste("Saving simulated parameters to ", file))
    saveRDS(simulatedParams, file)
  }
  
  DATA = list(y = y)
  CONSTANTS = c(CONSTANTS, list(t = t))
  return(list(data = DATA, constants = CONSTANTS))
}