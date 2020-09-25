library(nimble)
trueParams = list(
  nDays = 10,
  nObservations = 256,
  muY = c(  one = -80, two = -50,  three = -80  ),
  sigmaY = c(   one = 5,   two =  10,  three =  5  ),
  delta1 = 6,
  delta2 = 18,
  delta = 1/4
)

constants = list(
  nDays = 10,
  nObservations = 256,
  mu_y = c(  one = -80, two = -50,  three = -80  ),
  sigmaMu = c(   one = 10,   two =  10,  three =  10  ),
  delta1Mu = 8,
  delta2Mu = 16,
  sigmaDelta1 = 1.5,
  sigmaDelta2 = 1.5,
  ## Top level hyper parameter
  deltaMu = -1,
  sigmaDeltaMu = 1
)

modelCode <- nimbleCode(
  {

    ## DAY LIKELIHOODS
    for ( j in 1:nDays-1 ){
      # UNTIL PENULTIMATE DAY LIKELIHOODS
      for ( i in 1:nObservations){
          y[i,j] ~ dnorm(
            mu[ step( t[i] - delta1 ) + step( t[i] - delta2 ) + 1 ] ,
            tau[ step( t[i] - delta1 ) + step( t[i] - delta2 ) + 1 ] )
      }
    }
    ## ULTIMATE DAY LIKELIHOODS
    for ( i in 1:nObservations){
      y[i,nDays] ~ dnorm(
        mu[ step( t[i] - delta1 ) + step( t[i] - delta2 - delta) + 1 ] ,
        tau[ step( t[i] - delta1 ) + step( t[i] - delta2 - delta) + 1 ] )
    }


    ##PRIORS
    # INTERDAY PRIORS
    delta1 ~ dnorm( delta1Mu, 1 / (sigmaDelta1^2) ) 
    delta2 ~ dnorm( delta2Mu, 1 / (sigmaDelta2^2) )

    delta ~ dnorm( deltaMu, 1 / sigmaDeltaMu^2 )

    for(k in 1:3){
      mu[k] ~ dnorm( mu_y[k], 1 / (sigmaMu[k]^2) )
      tau[k] ~ dgamma(10, 2)
    }
  }
)