cat("\014")
library(nimble)


for ( i in 1:1 ){
    source("DataSimulation.R", local = TRUE)
    print("Running simulation")
    print(i)
    mcmc.out <- nimbleMCMC(
       code = nimCode, 
       constants = CONSTANTS, 
       data = DATA, 
       nchains = 3, 
       niter = 5000,
       thin = 10,
       summary = TRUE,
       )
    print("Simulation complete.")

    saveRDS(mcmc.out$summary$all.chains, paste( "Output", toString(i), "Summary.rds", sep=""))
}
