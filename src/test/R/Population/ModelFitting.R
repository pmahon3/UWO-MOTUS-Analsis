cat("\014")
library(parallel)
NCORES = 6 


runMCMC <- function(x){
  library(nimble)
  
  source("DataSimulation.R", local = TRUE)
    
  mcmc.out <- nimbleMCMC(
    code = nimCode, 
    constants = CONSTANTS, 
    data = DATA, 
    nchains = 3, 
    niter = 5000,
    summary = TRUE
  )

  result <- mcmc.out$summary$all.chains[1,]

  saveRDS(result, paste("Output", toString(x), ".rds", sep =""))
}

seed = set.seed(Sys.time())
cluster <- parallel::makeCluster(NCORES, setup_timeout = 0.5)

parLapply(cl = cluster, X=1:100, fun = runMCMC)
