source("Inputs.R")


y = array( dim = c(CONSTANTS$nBirds, CONSTANTS$nDays, CONSTANTS$nObservations))
t = array( dim = c(CONSTANTS$nBirds, CONSTANTS$nDays, CONSTANTS$nObservations))

## VECTORS FOR STORING PARAMETERS SIMULATED AT THE INDIVIDUAL LEVEL
simulated_delta_prime = vector(mode = "double", length = CONSTANTS$nBirds)
simulated_delta1 = vector(mode = "double", length = CONSTANTS$nBirds)
simulated_delta2 = vector(mode = "double", length = CONSTANTS$nBirds)


for ( bird in 1:CONSTANTS$nBirds ){

  ## INDIVIDUAL INTERDAY PARAMETERS
  delta1 <- rnorm(1, mean = mu_delta1, sd = delta1_sd)
  delta2 <- rnorm(1, mean = mu_delta2, sd = delta2_sd)

  # INDIVIDUAL DELTA PRIME
  delta_prime = rnorm(1,  mean = mu_delta_prime, sd = delta_prime_sd )

  simulated_delta1[bird] = delta1
  simulated_delta2[bird] = delta2
  simulated_delta_prime[bird] = delta_prime

  # INDIVIDUAL MODE SIGNAL STRENGTH PARAMETERS
  mu_y= vector(mode = "double", length = 3)
  tau_y = vector(mode = "double", length = 3)
  for ( j in 1:3 ){
    mu_y[j] <- rnorm(1, mean = mu_mu_y[j], sd = sd_mu_y[j])
    sd_y[j] <- sd_y[j]
  }

  ## ALL DAYS TO THE PENULTIMATE DAY
  for ( day in 1:CONSTANTS$nDays ){

    ## VECTORS FOR TIME STAMPS
    t1 <- seq( from = delta1-1, to = delta1+1, length.out = CONSTANTS$nObservations / 2)
    t2 <- seq( from = delta2-1, to = delta2+1, length.out = CONSTANTS$nObservations / 2)
    t[bird, day, ] = c(t1,t2)

    ## OBSERVATION SIMULATION
    for ( observation in 1:(CONSTANTS$nObservations)){
      if ( t[bird, day, observation] < delta1 ){
        mu = mu_y[1]
        sd = sd_y[1]
      }
      else if ( day == CONSTANTS$nDays && t[bird, day, observation] < delta2 + delta_prime){
        mu = mu_y[2]
        sd = sd_y[2]
      }
      else if ( t[bird, day, observation] < delta2 ){
        mu = mu_y[2]
        sd = sd_y[2]
      }
      else{
        mu = mu_y[3]
        sd = sd_y[3]
      }
      y[bird, day, observation] = rnorm(1, mu, sd)
    }
  }
}

simulated_params = cbind(simulated_delta_prime, simulated_delta1, simulated_delta2)

print(CONSTANTS)
DATA = c( list(y = y))
CONSTANTS = c(CONSTANTS, list(t = t))
