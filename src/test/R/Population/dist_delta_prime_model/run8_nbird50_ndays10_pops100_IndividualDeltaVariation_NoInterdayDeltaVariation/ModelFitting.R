library(parallel)

## CORE DETECTION
NCORES = 6

## PARALLEL FUNCTION DEFINITION
runMCMC <- function(x) {
  library(nimble)

  source("DataSimulation.R", local = TRUE)
  saveRDS(simulatedParams, paste("./results/SimulatedParams", toString(x), ".rds", sep = ""))

  print("Running simulation")
  print(x)

  model <- nimbleModel( code = modelCode, name = "model", constants = CONSTANTS, data = DATA)

  compiled <- compileNimble(model, showCompilerOutput = TRUE )
  configured <- configureMCMC( model, print = TRUE )
  configured$addMonitors(c("delta","delta_prime"))
  built <- buildMCMC(configured)
  compiled <- compileNimble( built, project = model, showCompilerOutput = TRUE )

  set.seed(Sys.time())
  compiled$run(5000)

  print("Simulation complete.")

  samples <- as.matrix(compiled$mvSamples)
  saveRDS(samples, paste( "./results/Samples", toString(x), ".rds", sep=""))
}

cl <- makeCluster(NCORES)

# RUN SIMULATION
results <- parLapply(cl, seq(1, NPOPS), runMCMC)
saveRDS(results, "./results/Results.RData")

# RESULTS ANALYSIS
source("Analysis.R")
