source("Inputs.R")

heaviside <- function(x){
  if (sign(x) > 0) return(1)
  else return(0)
}

y = array( dim = c(CONSTANTS$nBirds, CONSTANTS$nDays, CONSTANTS$nObservations ))

t = array( dim = c(CONSTANTS$nBirds, CONSTANTS$nDays, CONSTANTS$nObservations))

for ( bird in 1:CONSTANTS$nBirds ){
  
  ## INDIVIDUAL INTERDAY PARAMETERS
  
  mu_delta1 <- rnorm(1, mean = CONSTANTS$mu_mu_delta1, sd = 1/30)
  mu_delta2 <- rnorm(1, mean = CONSTANTS$mu_mu_delta2, sd = 1/30)

  mu = vector(mode = "double", length = 3)
  tau = vector(mode = "double", length = 3)
  
  for ( j in 1:3 ){
    mu[j] <- rnorm(1, mean = CONSTANTS$mu_mu_y[j], sd = CONSTANTS$sd_mu_y[j])
    tau[j] <- rgamma(1, shape = 10, rate = 1/2)
  }

  ## ALL DAYS TO THE PENULTIMATE DAY
  for ( day in 1:(CONSTANTS$nDays-1) ){
    
    ## INTRADAY PARAMETERS
    delta1 <- rnorm( 1, mean = mu_delta1, sd = 1/30 )
    delta2 <- rnorm( 1, mean = mu_delta2, sd = 1/30 )
    
    t1 <- seq( from = delta1-1, to = delta1+1, length.out = CONSTANTS$nObservations / 2)
    t2 <- seq( from = delta2-1, to = delta2+1, length.out = CONSTANTS$nObservations / 2)
    
    t[bird, day, ] = c(t1,t2)
    
    for ( observation in 1:CONSTANTS$nObservations ){
      
      mode <- heaviside( t[ bird, day, observation] - delta1) + heaviside( t[bird, day, observation] - delta2) + 1
      
      mu_y <- mu[mode]
    
      tau_y <- tau[mode]
      
      y[bird, day, observation] = rnorm(1, mean = mu_y, sd = 1 / sqrt(tau_y))
    }
  }
  
  ## ULTIMATE DAY
  
  ## INTRADAY PARAMETERS
  delta1 <- rnorm( 1, mean = mu_delta1, sd = 1/30)
  delta2 <- rnorm( 1, mean = mu_delta2, sd = 1/30)
  
  ## PENULTIMATE DAY BEDTIME SHIFT
  delta_prime = rnorm(1, mean = DELTA_PRIME, sd = CONSTANTS$sigma_delta_prime)
  
  t1f <- seq( from = delta1-1, to = delta1+1, length.out = CONSTANTS$nObservations / 2)
  t2f <- seq( from = delta2-1, to = delta2+1, length.out = CONSTANTS$nObservations / 2)
  
  t[bird, CONSTANTS$nDays, ] = c(t1f,t2f)
  
  for ( observation in 1:CONSTANTS$nObservations){

    mode <- heaviside( t[ bird, CONSTANTS$nDays, observation] - delta1) + heaviside( t[bird, CONSTANTS$nDays, observation] - delta2 - delta_prime) + 1
    mean = mu[mode]
    tau_y <- tau[mode]
    
    y[bird,CONSTANTS$nDays, observation] = rnorm(1, mean = mean, sd = 1 / sqrt(tau_y))
  }
}

DATA = c( list(y = y))
print(CONSTANTS)
CONSTANTS = c(CONSTANTS, list(t = t), mu_mu_mu_delta1 = CONSTANTS$mu_mu_delta1, mu_mu_mu_delta2 = CONSTANTS$mu_mu_delta2)
