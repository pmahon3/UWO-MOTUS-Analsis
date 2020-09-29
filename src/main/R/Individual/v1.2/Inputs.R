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
  sigmaDelta = 0.5
)

modelCode <- nimbleCode(
  {
    ## DAY LIKELIHOODS
    for ( i in 1:nDays-1){
      #LIKELIHOODS
      for ( j in 1:nObservations){
        y[i,j] ~ dnorm(
	  mu[ step( t[i,j] - delta1 ) + step( t[i,j] - delta2 ) + 1 ],
	  tau[step( t[i,j] - delta1 ) + step( t[i,j] - delta2 ) + 1 ]
	) 
      }
    }
    for ( j in 1:nObservations ){
      y[nDays, j] ~ dnorm(
        mu[ step( t[nDays, j] - delta1 ) + step( t[nDays, j] - delta2 - delta ) + 1],
	tau[step( t[nDays, j] - delta1 ) + step( t[nDays, j] - delta2 - delta ) + 1]
      )
    }
    #PRIORS
    delta1 ~ dnorm( muDelta1, 1 / sigmaDelta1^2 )
    delta2 ~ dnorm( muDelta2, 1 / sigmaDelta2^2 )
    delta <- dnorm( muDelta, 1 / sigmaDelta^2 )

    ## GLOBAL PRIORS
    for(k in 1:3){
      mu[k] ~ dnorm( mu_y[k], 1 / (sigmaMu[k]^2) )
      tau[k] ~ dgamma(10, 2)
    }
  }
)