library(nimble)
## TRUE HYPERPARAMETERS
trueParams <- list(
  # DELTA_PRIME HYPERPARAMETERS
  muDeltaPrime = 1/4,
  deltaPrimeSd = 1/120,

  # DELTA HYPERPARAMETERS
  muDelta1 = 6,
  muDelta2 = 18,
  delta1Sd = 1/30,
  delta2Sd = 1/30,

  # OBSERVATION MODEL SIGNAL STRENGTH HYPERPARAMETERS
  muMuY = c(-80, -50, -80),
  sdMuY = c(5, 10, 5),
  sdY = c(5, 15, 5)
)

## NIMBLE/BUGS/JAGS MODEL
modelCode <- nimbleCode(
  {
    ## LIKELIHOODS
    ## INDIVIDUAL LIKELIHOODS
    for ( i in 1:nBirds ){
      ## DAY LIKELIHOODS
      for ( j in 1:nDays-1 ){
        ## UNTIL PENULTIMATE DAY LIKELIHOODS
	for ( k in 1:nObservations ){
	  y[i,j,k] ~ dnorm(
	    mu_y[ i, step( t[i,j,k] - delta[i,1] ) + step( t[i,j,k] - delta[i, 2] ) + 1 ] ,
	    tau_y[ i, step( t[i,j,k] - delta[i,1] ) + step( t[i,j,k] - delta[i, 2] ) + 1 ]
	  )
   	}
      }
      ## ULTIMATE DAY LIKELIHOODS
      for ( k in 1:nObservations ){
	y[i,nDays,k] ~ dnorm(
	  mu_y[ i, step( t[i,nDays,k] - delta[i, 1] ) + step( t[i,nDays,k] - delta[i, 2] - delta_prime[i] ) + 1 ],
	  tau_y[i, step( t[i,nDays,k] - delta[i, 1] ) + step( t[i,nDays,k] - delta[i, 2] - delta_prime[i] ) + 1 ]
        )
      }
    }
    ## PRIORS
    ## INDIVIDUAL PRIORS
    for( i in 1:nBirds ){
      ## INTERDAY PRIORS
      for ( j in 1:2 ){
        delta[i, j] ~ dnorm( mu_delta[j], tau_delta[j] )
      }
      delta_prime[i] ~ dnorm( mu_delta_prime, tau_delta_prime  )
      for( j in 1:3 ){
        mu_y[ i, j ] ~ dnorm( mu_mu_y[j], 1/10 )
	tau_y[i, j ] ~ dgamma( shape = 5, rate = 1/2 )
      }
    }
    # MEAN DELTA PRIME PRIOR 
    mu_delta_prime ~ dnorm( mu_mu_delta_prime, 1/2 )
    tau_delta_prime ~ dgamma( shape = 1/2, rate = 1/2 )
    ## OTHER POPULATION PRIORS
    for( i in 1:2 ){
      mu_delta[i] ~ dnorm(mu_mu_delta[i], 1/2)
      tau_delta[i] ~ dgamma( shape = 1, rate = 1/2 )
    }
  }
)

## CONSTANTS USED IN FITTING
constants = list(
  # FACTORS AFFECTING SIMULATION SIZE
  nDays = 10,
  nBirds = 100,
  nObservations = 256,
  # HYPERPRIORS AFFECTING OBSERVATION FITTING
  # OBSERVATION MODE SIGNAL STRENGTH HYPERPRIORS
  mu_mu_y = c(-80, -50, -80),
  # DELTA1 AND DELTA2 HYPERPRIORS
  mu_mu_delta = c(7, 19),
  # DELTA_PRIME HYPERPRIORS
  mu_mu_delta_prime = 3/8
)