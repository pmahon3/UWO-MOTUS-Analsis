# HPDAnalysis performs the Highest Posterior Density calculations for the simulated mu_delta parameters

library(rjags)
PATH = "Out/HPD/"

# Matrices containing 0s and 1s, 1 if the parameter is in the 0.95 highest density interval, 0 if not

delta_prime_results <- matrix( nrow = NPOPS, ncol = NCHAINS )
delta_prime_results <- matrix( nrow = NPOPS, ncol = NCHAINS ) 

# For each simulation calculate the HPD for each chain, take the average of the bounds and consider the coverage, and fill in the results matrices
for ( i in 1:NPOPS){
  
  dat <- HPDinterval(readRDS(paste(PATH, "HPD", i, ".RDS", sep = "")), prob = 0.95 )
  delta_prime_lower_avg <- (dat[[1]][1] + dat[[2]][1] + dat[[3]][1])/3
  delta_prime_upper_avg <- (dat[[1]][2] + dat[[2]][2] + dat[[3]][2])/3
  
  for ( j in 1:NCHAINS ){
    
    if ( delta_prime_lower_avg <= DELTA_PRIME && delta_prime_upper_avg >= DELTA_PRIME ){
      delta_prime_results[i, j] <- 1
    }
    else{
      delta_prime_results[i, j] <- 0
    }
  }
}

delta_prime_result <- sum(delta_prime_results)/(NPOPS*NCHAINS)

print(paste("DELTA_PRIME Coverage:" , delta_prime_result , sep = " "))