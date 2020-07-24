
source("Inputs.R")

NPOPS = 8

coverage_results <- array( dim = c(NPOPS + 1, 1) )
mu_delta_prime_coverage = 0

for ( i in 1: NPOPS ){

    ## INDIVIDUAL DELTA_PRIME COVERAGE
    actualParams <- readRDS(paste("SimulatedParams", toString(i), ".RDS", sep = "" ))
    simulatedParams <- readRDS(paste("Summary", toString(i), ".rds", sep = "" ))
    individual_delta_prime_coverage = 0
   
    for ( j in 1:CONSTANTS$nBirds ){

    	simulated_delta_prime_lower <- simulatedParams$summary$all.chains[j, 4]
	simulated_delta_prime_upper <- simulatedParams$summary$all.chains[j, 5]
	actual_delta_prime <- actualParams[j,1]

	if ( actual_delta_prime >= simulated_delta_prime_lower && actual_delta_prime <= simulated_delta_prime_upper ){
	   individual_delta_prime_coverage = individual_delta_prime_coverage + 1
	}
    }

    ## MU_DELTA_PRIME COVERAGE
    simulated_mu_delta_prime_lower = simulatedParams$summary$all.chains[CONSTANTS$nBirds + 1, 4]
    simulated_mu_delta_prime_upper = simulatedParams$summary$all.chains[CONSTANTS$nBirds + 1, 5]
    
    if ( simulated_mu_delta_prime_lower <= mu_delta_prime && simulated_mu_delta_prime_upper >= mu_delta_prime ){
       mu_delta_prime_coverage = mu_delta_prime_coverage + 1
    }

    individual_delta_prime_coverage = individual_delta_prime_coverage / CONSTANTS$nBirds
    coverage_results[i] = individual_delta_prime_coverage
}

mu_delta_prime_coverage
coverage_results[NPOPS + 1] = mu_delta_prime_coverage / NPOPS
coverage_results
saveRDS(coverage_results, "Coverage.rds")