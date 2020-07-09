library(parallel)

## CORE DETECTION AND NUMBER OF POPULATIONS ASSIGNMENT
NCORES = detectCores()
NPOPS = NCORES

## PARALLEL FUNCTION DEFINITION
runMCMC <- function(x){
  library(nimble)

  # SOURCE DATA SIMULATION AND INPUT SCRIPT ( REFERENCED IN DATASIMULATION )
  source("DataSimulation.R", local = TRUE)
  saveRDS(simulated_params, paste("SimulatedParams", toString(x), ".RDS", sep = ""))

  # MONITOR SELECTION
  monitors_vec = vector( mode = "list", length = CONSTANTS$nBirds + 1)
  monitors_vec[1] = "mu_delta_prime"
  for ( i in 2:CONSTANTS$nBirds+1 ) {
      monitors_vec[i] = paste("delta_prime[", toString(i), "]", sep="")
  }

  ## RUN FITTING
  mcmc.out <- nimbleMCMC(
    code = nimCode,
    constants = CONSTANTS,
    data = DATA,
    nchains = 3,
    niter = 5000,
    thin = 10,
    monitors = monitors_vec,
    summary = TRUE
  )

  saveRDS(mcmc.out, paste("Summary", toString(x), ".rds", sep="" ))
}

## SET SEED AND BUILD CLUSTER
seed = set.seed(Sys.time())
cl <- makeCluster(NCORES)

# RUN SIMULATION
results <- parLapply(cl, seq(1,NPOPS), runMCMC)
saveRDS(results, "Results.RData")
print(results)
