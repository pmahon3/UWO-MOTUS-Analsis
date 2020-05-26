cat("\014")
library(nimble)


for ( i in 1:100 ){
    source("DataSimulation.R", local = TRUE)
    print("Running simulation")
    print(i)
    CONSTANTS$mu_delta_prime = CONSTANTS$mu_delta_prime + CONSTANTS$mu_delta_prime * 0.25
    mcmc.out <- nimbleMCMC(
       code = nimCode, 
       constants = CONSTANTS, 
       data = DATA, 
       nchains = 3, 
       niter = 5000,
       summary = TRUE
    )
    print("Simulation complete.")

    saveRDS(mcmc.out$summary$all.chains[1,], paste( "Output", toString(i), ".rds", sep=""))
}
