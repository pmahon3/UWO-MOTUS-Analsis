cat("\014")
library(parallel)
NCORES = detectCores()
NPOPS = NCORES*4


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
  saveRDS(summary, paste("Summary", toString(x), ".rds", sep=""))
}

seed = set.seed(Sys.time())

cl <- makeCluster(NCORES)

results <- parLapply(cl, seq(1,NPOPS), runMCMC)

saveRDS(vector(mode="double", length = 5),"CombinedSummary.rds")
for ( i in 1:NPOPS ){
    saveRDS(rbind(readRDS("CombinedSummary.rds"), readRDS(paste("Summary", toString(i), ".rds", sep=""))), "CombinedSummary.rds")
    file.remove(paste("Summary", toString(i), ".rds", sep=""))
}

saveRDS(results, "AllResults.rds")