cat("\014")
library(parallel)
NCORES = detectCores()
NPOPS = NCORES


runMCMC <- function(x){
  library(nimble)
  
  source("DataSimulation.R", local = TRUE)
    
  mcmc.out <- nimbleMCMC(
    code = nimCode, 
    constants = CONSTANTS, 
    data = DATA, 
    nchains = 3, 
    niter = 5000,
    summary = TRUE,
    monitors = "avg_delta_prime"
  )

  summary  <- mcmc.out$summary$all.chains
  samples <- mcmc.out$mvSamples

  saveRDS(samples, paste("Samples", toString(x), ".rds", sep=""))
  saveRDS(summary, paste("Summary", toString(x), ".rds", sep=""))
}

seed = set.seed(Sys.time())

cl <- makeCluster(NCORES)

results <- parLapply(cl, seq(1,NPOPS), runMCMC)

saveRDS(results, "AllResults.rds")