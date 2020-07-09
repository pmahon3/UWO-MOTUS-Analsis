cat("\014")
library(parallel)
NCORES = detectCores()
NPOPS = NCORES

source("DataSimulation.R")


runMCMC <- function(x){
  library(nimble)

  # SOURCE DATA SIMULATION AND INPUT SCRIPT ( REFERENCED IN DATASIMULATION )
  source("DataSimulation.R", local = TRUE)
 

  # MONITOR SELECTION
  monitors_vec = vector( mode = "list", length = CONSTANTS$nBirds + 1)
  monitors_vec[1] = "mu_delta_prime"
  for ( i in 2:CONSTANTS$nBirds+1 ) {
      monitors_vec[i] = paste("delta_prime[", toString(i), "]", sep="")
  }

  print(monitors_vec)

  mcmc.out <- nimbleMCMC(
    code = nimCode, 
    constants = CONSTANTS, 
    data = DATA, 
    nchains = 3, 
    niter = 5000,
    thin = 10,
    monitors = monitors_vec,
    samples = TRUE,
    summary = TRUE
  )

  summary  <- mcmc.out

  saveRDS(delta_prime_actual, paste("ActualDeltaPrime", toString(x), ".rds", sep="" ))
  save(summary, file = paste("Summary", toString(x), ".RData", sep=""))
}

seed = set.seed(Sys.time())

cl <- makeCluster(NCORES)

results <- parLapply(cl, seq(1,NPOPS), runMCMC)

print(results)