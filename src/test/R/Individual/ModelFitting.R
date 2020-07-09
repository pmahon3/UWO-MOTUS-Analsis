cat("\014")
library(parallel)
NCORES = detectCores()
NINDIV = NCORES

source("DataSimulation.R")


runMCMC <- function(x){
  library(nimble)

  # SOURCE DATA SIMULATION AND INPUT SCRIPT ( REFERENCED IN DATASIMULATION )
  source("DataSimulation.R", local = TRUE)

  mcmc.out <- nimbleMCMC(
    code = nimCode, 
    constants = CONSTANTS, 
    data = DATA, 
    nchains = 3,
    niter = 5000,
    thin = 10,
    samples = TRUE,
    summary = TRUE
  )

  summary  <- mcmc.out

  save(summary, file = paste("Summary", toString(x), ".RData", sep=""))
}

seed = set.seed(Sys.time())

cl <- makeCluster(NCORES)

results <- parLapply(cl, seq(1,NINDIV), runMCMC)

print(results)