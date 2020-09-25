library(nimble)
trueParams = list(
  nDays = 10,
  nObservations = 256,
  muY = c(  one = -80, two = -50,  three = -80  ),
  sigmaY = c(   one = 5,   two =  10,  three =  5  ),
  delta1Mu = 6,
  delta2Mu = 18,
  deltaMu = 0.25
)

constants = list(
  nBirds = 25
  nDays = 10,
  nObservations = 256,
  
  mu_y = c(  one = -80, two = -50,  three = -80  ),
  sigmaMu = c(   one = 20,   two =  20,  three =  20  ),
  
  sigmaDelta1 = 1.5,
  sigmaDelta2 = 1.5,
  
  delta1MuMu = 6
  delta2MuMu = 18
  deltaMuMu = 0.25,
  sigmaDelta1Mu = 1,
  sigmaDelta2Mu = 1,
  sigmaDeltaMu = 0.5
)

modelCode <- nimbleCode(
  {

    ## DAY LIKELIHOODS
    for ( i in 1:nBirds ){
      for ( j in 1:nDays-1 ){
        # UNTIL PENULTIMATE DAY LIKELIHOODS
        for ( k in 1:nObservations){
          y[i,j,k] ~ dnorm(
            mu[ step( t[i,j,k] - delta1 ) + step( t[i,j,k] - delta2 ) + 1 ] ,
            tau[ step( t[i,j,k] - delta1 ) + step( t[i,j,k] - delta2 ) + 1 ] )
        }
      }
      ## ULTIMATE DAY LIKELIHOODS
      for ( k in 1:nObservations){
        y[i,nDays, k] ~ dnorm(
          mu[ step( t[i,j,k] - delta1 ) + step( t[i,j,k] - delta2 - delta) + 1 ] ,
          tau[ step( t[i,j,k] - delta1 ) + step( t[i,j,k] - delta2 - delta) + 1 ] )
      }
    


      ##PRIORS
      # INTERDAY PRIORS
      delta1 ~ dnorm( muDelta1, 1 / (sigmaDelta1^2) ) 
      delta2 ~ dnorm( muDelta2, 1 / (sigmaDelta2^2) )

      delta ~ dnorm( muDelta, 1 / sigmaDelta^2 )
    }

    muDelta1 ~ dnorm( muMudelta1, 1 / (sigmaMuDelta1^2) )
    muDelta2 ~ dnorm( muMudelta2, 1 / (sigmaMuDelta2^2) )
    muDelta ~ dnorm( muMuDelta, 1 / (sigmaMuDelta^2) )

    for(k in 1:3){
      mu[k] ~ dnorm( mu_y[k], 1 / (sigmaMu[k]^2) )
      tau[k] ~ dgamma(10, 2)
    }
  }
)