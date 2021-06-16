

dataSimulation <- function(x, CONSTANTS, TRUEPARAMS, saveDat){

  ## Initialize storage matrices and vectors
  y <- t <- p <- array( dim = c(CONSTANTS$nBirds, CONSTANTS$nDays, CONSTANTS$nObservations))
  muDelta <- array(dim = c(CONSTANTS$nBirds, 2))
  delta <- array(dim = c(CONSTANTS$nBirds, CONSTANTS$nDays, 2))
  delta.prime <- vector(mode = "double", length = CONSTANTS$nBirds)
  muY <- array(dim = c(CONSTANTS$nBirds, CONSTANTS$nDays, 3))
  sigmaY <- array(dim = c(CONSTANTS$nBirds, CONSTANTS$nDays, 3))

  # Loop over birds
  for ( bird in 1:CONSTANTS$nBirds ){
    
    # Mean change points, delta prime, and delta standard deviations
      muDelta[bird,] <- rnorm(2,
                               mean = TRUEPARAMS$muMuDelta,
                               sd = TRUEPARAMS$sigmaMuDelta)

      delta.prime[bird] <- rnorm(1,
                                mean = TRUEPARAMS$muDelta.prime,
                                sd = TRUEPARAMS$sigmaDelta.prime )

    # All days to the penultimate day
    for ( day in 1:CONSTANTS$nDays ){
      
                                        # Signal strength means and standard deviations
        muY[bird,day,] <- rnorm(3,
                                mean = trueParams$muMuY,
                                sd = TRUEPARAMS$sigmaMuY)
        
        sigmaY[bird,day,] <- exp(rnorm(3,
                                    mean = TRUEPARAMS$muSigmaY,
                                    sd = TRUEPARAMS$sigmaSigmaY))
      
      # Simulate change points
        delta[bird,day,] <- rnorm(2,
                                 mean = muDelta[bird,],
                                 sd = TRUEPARAMS$sigmaDelta)
        
      # Vectors for the time stamps
        t[bird, day,]  <- c(seq(from = TRUEPARAMS$muMuDelta[1]-1,
                                to = TRUEPARAMS$muMuDelta[1]+1,
                                length.out = CONSTANTS$nObservations / 2),
                            seq(from = TRUEPARAMS$muMuDelta[2]-1,
                                to = TRUEPARAMS$muMuDelta[2]+1,
                                length.out = CONSTANTS$nObservations / 2))

      # Period
        p[bird,day,] <- 1 +
            (t[bird,day,] > delta[bird, day, 1]) +
            (t[bird,day,] > (delta[bird,day, 2] + delta.prime[bird] * (day == CONSTANTS$nDays)))

      # Signal strength
        y[bird, day, ] <- rnorm(length(t[bird,day,]),
                               mean = muY[bird, day, p[bird, day,]],
                               sd = sigmaY[bird, day, p[bird, day,]])
    }
  }

    simulatedParams <- list('delta'=delta,
                            'muDelta'=muDelta,
                            'deltaPrime'=delta.prime,
                            'muY'=muY,
                            'sigmaY'=sigmaY)

  if (saveDat){
    file =  paste("./results/data/simulatedParams", toString(x), ".rds", sep = "")
    print(paste("Saving simulated parameters to ", file))
    saveRDS(simulatedParams, file)
  }
  
    DATA = list(y = y,
                t = t)

    return(DATA)
}
