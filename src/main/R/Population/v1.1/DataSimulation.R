

dataSimulation <- function(x, CONSTANTS, TRUEPARAMS, saveDat){

  ## Initialize storage matrices and vectors
  y = array( dim = c(CONSTANTS$nBirds, CONSTANTS$nDays, CONSTANTS$nObservations))
  t = array( dim = c(CONSTANTS$nBirds, CONSTANTS$nDays, CONSTANTS$nObservations))
  muDelta = array(dim = c(CONSTANTS$nBirds, 2))
  sigmaDelta = array(dim = c(CONSTANTS$nBirds, 2))
  delta = array(dim = c(CONSTANTS$nBirds, CONSTANTS$nDays, 2))
  deltaPrime = vector(mode = "double", length = CONSTANTS$nBirds)
  muY = array(dim = c(CONSTANTS$nBirds, CONSTANTS$nDays, 3))
  sdY = array(dim = c(CONSTANTS$nBirds, CONSTANTS$nDays, 3))

  # Loop over birds
  for ( bird in 1:CONSTANTS$nBirds ){
    
    # Mean change points, delta prime, and delta standard deviations
    for( i in 1:2){
      muDelta[bird,1] <- rnorm(1, mean = TRUEPARAMS$muMuDelta1, sd = TRUEPARAMS$sigmaMuDelta1)
      muDelta[bird,2] <- rnorm(1, mean = TRUEPARAMS$muMuDelta2, sd = TRUEPARAMS$sigmaMuDelta2)
      sigmaDelta[bird,1] <- rht(1, nu = TRUEPARAMS$dfSigmaDelta[1], sigma = TRUEPARAMS$scaleSigmaDelta[1])
      sigmaDelta[bird,2] <- rht(1, nu = TRUEPARAMS$dfSigmaDelta[2], sigma = TRUEPARAMS$scaleSigmaDelta[2])
    }
    deltaPrime[bird] = rnorm(1,  mean = TRUEPARAMS$muDelta.prime, sd = TRUEPARAMS$sigmaDelta.prime )

    
    # All days to the penultimate day
    for ( day in 1:CONSTANTS$nDays ){
      
      # Signal strength means and standard deviations
      for ( i in 1:3 ){
        muY[bird,day,i] <- rnorm(1, mean = TRUEPARAMS$muMuY[i], sd = TRUEPARAMS$sigmaMuY[i])
        sdY[bird,day,i] <- rht(1, nu = TRUEPARAMS$dfSdY[i], sigma = TRUEPARAMS$scaleSdY[i])
      }
      
      # Simulate change points
      delta[bird,day,1] = rnorm(1, mean = muDelta[bird,1], sd = sigmaDelta[bird,1])
      delta[bird,day,2] = rnorm(1, mean = muDelta[bird,2], sd = sigmaDelta[bird,2])
      # Vecotrs for the time stamps
      t1 <- seq( from = delta[bird,day,1]-1, to = delta[bird,day,1]+1, length.out = CONSTANTS$nObservations / 2)
      t2 <- seq( from = delta[bird,day,2]-1, to = delta[bird,day,2]+1, length.out = CONSTANTS$nObservations / 2)
      t[bird, day, ] = c(t1,t2)

      # Observation mode logic
      for ( observation in 1:(CONSTANTS$nObservations)){
        if ( t[bird, day, observation] < delta[bird,day,1] ){
          mu = muY[bird,day,1]
          sd = sdY[bird,day,1]
        }
        else if ( day == CONSTANTS$nDays && t[bird, day, observation] < delta[bird,day,2] + deltaPrime){
          mu = muY[bird,day,2]
          sd = sdY[bird,day,2]
        }
        else if ( t[bird, day, observation] < delta[bird,day,2] ){
          mu = muY[bird,day,2]
          sd = sdY[bird,day,2]
        }
        else{
          mu = muY[bird,day,3]
          sd = sdY[bird,day,3]
        }
        y[bird, day, observation] = rnorm(1, mu, sd)
      }
    }
  }

  simulatedParams = list('delta'=delta,'muDelta'=muDelta,'sigmaDelta'=sigmaDelta,'deltaPrime'=deltaPrime,'muY'=muY,'sdY'=sdY)

  if (saveDat){
    file =  paste("./results/data/simulatedParams", toString(x), ".rds", sep = "")
    print(paste("Saving simulated parameters to ", file))
    saveRDS(simulatedParams, file)
  }
  
  DATA = list(y = y)
  CONSTANTS = c(CONSTANTS, list(t = t))
  return(list(data = DATA, constants = CONSTANTS))
}