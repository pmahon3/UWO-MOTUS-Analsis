library(nimble)
trueParams = list(
  nDays = 10,
  nObservations = 600,
  muY = c(  one = -80, two = -50,  three = -80  ),
  sigmaY = c(   one = 5,   two =  10,  three =  5  ),

  muDelta1 = 6,
  muDelta2 = 18,
  sigmaDelta1 = 0.1,
  sigmaDelta2 = 0.1,

  delta = 1/4
)

constants = list(
  nDays = 10,
  nObservations = 600,
  mu_y = c(  one = -80, two = -50,  three = -80  ),
  sigmaMu = c(   one = 10,   two =  10,  three =  10  ),

  muDelta1 = 6,
  muDelta2 = 18,
  sigmaDelta1 = 1,
  sigmaDelta2 = 1,

  muDelta = 0.25,
  sigmaMuDelta = 0.5
)

modelCode <- nimbleCode(
  {
    ## UP TO PENULTIMATE DAY
    for ( i in 1:nDays-1 ){
      #LIKELIHOODS
      for ( j in 1:nObservations){
        ## Identify period of day
        k[i,j] <- step( t[i,j] - delta1[i] ) + step( t[i,j] - delta2[i] ) + 1

        ## Model response
        y[i,j] ~ dnorm( mu[k[i,j]],tau[k[i,j]])
      }
      
      #PRIORS
      delta1[i] ~ dnorm( etaDelta1, tauDelta1^2 )
      delta2[i] ~ dnorm( etaDelta2, tauDelta2^2 )
    }

    ## PENULTIMATE DAY
    # LIKELIHOODS
    for ( j in 1:nObservations ){
      ## Identify period of day
      k[nDays,j] <- step( t[nDays,j] - delta1[nDays] ) +
        step( t[nDays,j] - delta2[nDays] - delta ) + 1

      ## Model response
      y[nDays,j] ~ dnorm(mu[k[nDays,j]], tau[k[nDays,j]])
    }
    
    # PRIORS
    delta1[nDays] ~ dnorm( etaDelta1, tauDelta1 )
    delta2[nDays] ~ dnorm( etaDelta2, tauDelta2 )
    delta ~ dnorm( muDelta, 1 / sigmaMuDelta^2 )

    ## HYPERPRIORS
    etaDelta1 ~ dnorm(muDelta1, 1/ sigmaDelta1^2)
    etaDelta2 ~ dnorm(muDelta2, 1/ sigmaDelta2^2)

    tauDelta1 ~ dgamma(.01,.01)
    tauDelta2 ~ dgamma(.01,.01)
    
    ## GLOBAL PRIORS
    for(k in 1:3){
      mu[k] ~ dnorm( mu_y[k], 1 / (sigmaMu[k]^2) )
      tau[k] ~ dgamma(.01, .01)
    }
  }
)
