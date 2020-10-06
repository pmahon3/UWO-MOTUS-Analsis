genInits <- function(data,constants){
  
  ## Set initial changepoints to middle of windows
  delta <- rbind(rep(mean(constants$window1, constants$nDays)),
                 rep(mean(constants$window2, constants$nDays)))

  ## Set initial delta.prime to zero
  delta.prime <- 0

  ## Set initial means and variances for strength
  mu <- matrix(NA, constants$nDays, 3)
  sigma <- matrix(NA, constants$nDays, 3)

  for(i in 1:nDays){
    ## Identify period for each observation
    period <- (constants$t[i,] > delta[1,i]) +
      (constants$t[i,] > delta[2,i]) + 1

    ## Compute means and standard deviations
    mu[i,] <- sapply(1:3, function(k) mean(data$y[i,period == k]))
    sigma[i,] <- sapply(1:3, function(k) sd(data$y[i,period == k]))
  }

  ## Return list of initial values
  list(delta = delta,
       mu = mu,
       mu_y = apply(mu, 2, mean),
       tau_y = 1/apply(mu, 2, var),
       sigma = apply(sigma,2,mean))
}
      
                     
  
