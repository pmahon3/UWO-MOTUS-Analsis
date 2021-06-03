library(foreach)
library(doParallel)
library(nimble)
library(HDInterval)
library(extraDistr)

NCORES = 6
NPOPS = 6

## Parallel function definition
runMCMC <- function(x) {
  # Output logging
  messagelog <- file(paste("./results/messages/messages", toString(x), ".txt", sep = ""), open = "wt")
  outputlog <- file(paste("./results/output/output", toString(x), ".txt", sep = "" ), open = "wt")
  
  # Simulation
  print(paste("Simulation", toString(x), sep = " "))
  print("------------------------------------------")
  dataAndConstants <- dataSimulation(x, constants, trueParams, TRUE)
  saveRDS(dataAndConstants[["constants"]], paste("./results/data/populationInputs", toString(x), ".rds", sep = ""))
  model <- nimbleModel( code = modelCode, name = "model", constants = dataAndConstants[["constants"]], data = dataAndConstants[["data"]], calculate = FALSE)
  configured <- configureMCMC(model)
  configured$resetMonitors()
  configured$setSamplers(c("muDelta.prime", "delta.prime", "muMuDelta", "muDelta", "delta", "tauDelta", "muMuY", "muY"))
  configured$addMonitors(c("muDelta.prime","delta.prime", "muMuDelta", "muDelta", "delta", "tauDelta", "muMuY", "muY"))
  configured$setThin(10)
  built <- buildMCMC(configured)
  compiled <- compileNimble( model, built, showCompilerOutput = TRUE )
  compiled$built$run(niter = 5000)
  
  # Save results
  samples <- as.matrix(compiled$built$mvSamples)
  saveRDS(samples, paste( "./results/samples/population", toString(x), ".rds", sep=""))
  print(paste("Simulation", toString(x), "complete.", sep = " "))
  print("-------------------------------------------")
}

source("Inputs.R")
source("DataSimulation.R")
source("Model.R")
registerDoParallel(NCORES)

## Run in parallel
for ( i in 1:NPOPS ) {
  runMCMC(i)
}
