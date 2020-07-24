cat("\014")
library(parallel)
NCORES = detectCores()
NPOPS = NCORES

source("DataSimulation.R")

runMCMC <- function(x){
  library(nimble)
  
  source("DataSimulation.R", local = TRUE)
  saveRDS("", "CombinedSummary.rds")

  mcmc.out <- nimbleMCMC(
    code = nimCode, 
    constants = CONSTANTS, 
    data = DATA, 
    nchains = 3, 
    niter = 5000,
    summary = TRUE
  )

  summary  <- mcmc.out$summary
  saveRDS(summary, paste("Summary", toString(x), ".rds", sep=""))
  saveRDS(cbind(readRDS("CombinedSummary.rds"), readRDS(paste("Summary", toString(x), ".rds", sep=""))), "CombinedSummary.rds")
}

seed = set.seed(Sys.time())

cl <- makeCluster(NCORES)

results <- parLapply(cl, seq(1,NPOPS), runMCMC)

print(results)