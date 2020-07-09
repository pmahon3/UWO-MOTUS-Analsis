library(parallel)

## CORE DETECTION AND NUMBER OF POPULATIONS ASSIGNMENT
NCORES = detectCores()
NPOPS = 20

## PARALLEL FUNCTION DEFINITION
runMCMC <- function(x){
  library(nimble)

  source("DataSimulation.R", local = TRUE)
  saveRDS(simulated_params, paste("./results/SimulatedParams", toString(x), ".rds", sep = ""))

  print("Running simulation")
  print(x)

  model <- nimbleModel( code = nimCode, name = "model", constants = CONSTANTS )

  compiledModel <- compileNimble(model, showCompilerOutput = TRUE )
  configuredModel <- configureMCMC( model, print = TRUE )
  modelMCMC <- buildMCMC(configuredModel)
  compiledMCMC <- compileNimble( modelMCMC, project = model)
  set.seed(Sys.time())
  compiledMCMC$run(5000)

  print("Simulation complete.")

  samples <- compiledMCMC$summary
  saveRDS(samples, paste( "./results/Summary", toString(x), ".rds", sep=""))
}

cl <- makeCluster(NCORES)

# RUN SIMULATION
results <- parLapply(cl, seq(1,NPOPS), runMCMC)
saveRDS(results, "./results/Results.RData")

# RESULTS ANALYSIS
source("Analysis.R")
