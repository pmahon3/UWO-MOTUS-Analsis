cat("\014")
library(parallel)
NCORES = detectCores()
NPOPS = NCORES

source("DataSimulation.R")

saveRDS(vector(mode="double", length = 5), "CombinedSummary.rds")

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
    monitors = "delta_prime"
  )

  summary  <- mcmc.out$summary$all.chains
  saveRDS(summary, paste("Summary", toString(x), ".rds", sep=""))
  saveRDS(rbind(readRDS("CombinedSummary.rds"), readRDS(paste("Summary", toString(x), ".rds", sep=""))), "CombinedSummary.rds")
}

seed = set.seed(Sys.time())

cl <- makeCluster(NCORES)

results <- parLapply(cl, seq(1,NPOPS), runMCMC)

print(results)