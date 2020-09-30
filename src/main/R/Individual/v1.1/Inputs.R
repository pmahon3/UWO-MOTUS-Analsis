library(nimble)
trueParams = list(
  nDays = 10,
  nObservations = 600,
  muY = c(  one = -80, two = -50,  three = -80  ),
  sigmaY = c(   one = 5,   two =  10,  three =  5  ),

  muDelta1 = 6,
  muDelta2 = 18,
  sigmaDelta1 = .1,
  sigmaDelta2 = .2,

  delta = 1/4
)

constants = list(
  nDays = 10,
  nObservations = 600,
  mu_y = c(  one = -80, two = -50,  three = -80  ),
  sigmaMu = c(   one = 10,   two =  10,  three =  10  ),

  muDelta = c(6, 18),
  sigmaDelta = c(1, 1),

  muDelta.prime = 0.25,
  sigmaMuDelta.prime = 0.5
)

modelCode <- nimbleCode(
  {
    #LIKELIHOODS

    for ( i in 1:nDays ){
      for ( j in 1:nObservations){
        ## Identify period of day
        k[i,j] <- step( t[i,j] - delta[1,i] ) +
          step( t[i,j] - delta[2,i] - delta.prime * step(days - nDays)) + 1

        ## Model response
        y[i,j] ~ dnorm( mu[k[i,j]],tau[k[i,j]])
      }
    }

    # PRIORS
    for ( i in 1:nDays ){
      for(k in 1:2){
        delta[k,nDays] ~ dnorm( etaDelta[k], tauDelta[k] )
      }
    }
    delta.prime ~ dnorm( muDelta.prime, 1 / sigmaMuDelta.prime^2 )

    ## HYPERPRIORS
    for(k in 1:2){
      etaDelta[k] ~ dnorm(muDelta[k], 1/ sigmaDelta[k]^2)
      tauDelta[k] ~ dgamma(.01,.01)
    }
    
    ## GLOBAL PRIORS
    for(k in 1:3){
      mu[k] ~ dnorm( mu_y[k], 1 / (sigmaMu[k]^2) )
      tau[k] ~ dgamma(.01, .01)
    }
  }
)
