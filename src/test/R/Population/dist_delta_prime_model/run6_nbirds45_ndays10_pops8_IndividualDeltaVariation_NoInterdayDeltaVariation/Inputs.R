library(nimble)




## NIMBLE/BUGS/JAGS MODEL
nimCode <- nimbleCode(
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
      delta[i, 1] ~ dnorm( mu_delta1, tau_delta1 )
      delta[i, 2] ~ dnorm( mu_delta2, tau_delta2 )

			delta_prime[i] ~ dnorm( mu_delta_prime, tau_delta_prime )

      for( j in 1:3 ){
        mu_y[ i, j ] ~ dnorm( mu_mu_y[j], tau_mu_y[j] )
				tau_y[i, j ] ~ dgamma( tau_y_shape[j], tau_y_rate[j] )
      }
    }
    # MEAN DELTA PRIME PRIOR ( HYPERPRIOR OF PRIMARY INTEREST )
    mu_delta_prime ~ dnorm( mu_mu_delta_prime, tau_mu_delta_prime)
    tau_delta_prime ~ dgamma(tau_delta_prime_shape, tau_delta_prime_rate)

    ## OTHER POPULATION PRIORS
    mu_delta1 ~ dnorm(mu_mu_delta1, tau_mu_delta1)
    mu_delta2 ~ dnorm(mu_mu_delta2, tau_mu_delta2)
		tau_delta1 ~ dgamma(tau_delta1_shape, tau_delta1_rate)
		tau_delta2 ~ dgamma(tau_delta2_shape, tau_delta2_rate)
  }
)



## TARGET HYPERPARAMETERS: NOT VISIBLE TO JAGS CODE, ONLY VISIBLE TO DATA SIMULATION
# DELTA_PRIME HYPERPARAMETERS
mu_delta_prime = 1/4
delta_prime_sd = 1/120

# DELTA HYPERPARAMETERS
mu_delta1 = 6
mu_delta2 = 18
delta1_sd = 1/30
delta2_sd = 1/30

# OBSERVATION MODEL SIGNAL STRENGTH HYPERPARAMETERS
mu_mu_y = c(one = -80, two = -50, three = -80)
sd_mu_y = c(one = 5, two = 5, three = 5)
sd_y = c( one = 3, two = 5, three = 3)

## CONSTANTS USED IN FITTING
CONSTANTS = list(

  # FACTORS AFFECTING SIMULATION SIZE
  nDays = 10,
  nBirds = 45,
  nObservations = 512,

  # HYPERPRIORS AFFECTING OBSERVATION FITTING
	# OBSERVATION MODE SIGNAL STRENGTH HYPERPRIORS
  mu_mu_y = c(one = -80, two = -50,  three = -80),
  tau_mu_y = c(one = 1/10, two = 1/10, three = 1/10),

	tau_y_rate = c( one = 10, two = 10, three = 10),
	tau_y_shape = c( one = 1/2, two = 1/2, three = 1/2),

	# DELTA1 AND DELTA2 HYPERPRIORS
  mu_mu_delta1 = 7,
  mu_mu_delta2 = 19,
	tau_mu_delta1 = 1,
	tau_mu_delta2 = 1,

	tau_delta1_shape = 1,
	tau_delta1_rate = 1/2,
	tau_delta2_shape = 1,
	tau_delta2_rate = 1/2,

	# DELTA_PRIME HYPERPRIORS
  mu_mu_delta_prime = 3/8,
  tau_mu_delta_prime = 1,

	tau_mu_delta_prime_shape = 1,
	tau_mu_delta_prime_rate = 1/2,

	tau_delta_prime_shape = 1,
	tau_delta_prime_rate = 1/2
)
