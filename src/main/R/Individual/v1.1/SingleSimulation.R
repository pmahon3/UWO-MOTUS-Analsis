## Load packages
library(parallel)
library(HDInterval)

## Simulation parameters
NBIRDS = 100

## Load input files
source("Inputs.R")
source("DataSimulation.R")

## PARALLEL FUNCTION DEFINITION
runMCMC <- function(x) {
  print(paste("Simulation", toString(x), sep = " "))
  print("------------------------------------------")
  
  ##messagelog <- file(paste("messages", toString(x), ".txt", sep = ""), open = "wt")
  ##outputlog <- file(paste("output", toString(x), ".txt", sep = "" ), open = "wt")
  ##sink(file = messagelog, type = "message")
  ##sink(file = outputlog, type = "output")

  ## Load packages
  library(nimble)

  ## Simulate data
  dataAndConstants <- dataSimulation(x, constants, trueParams)

  ## Save data
  saveRDS(dataAndConstants[["data"]]$ds, paste("./results/data/paramsBird", toString(x), ".rds", sep = ""))

  ## Configure nimble model
  model <- nimbleModel( code = modelCode,
                       name = "model",
                       constants = dataAndConstants[["constants"]],
                       data = dataAndConstants[["data"]],
                       calculate = FALSE)
  configured <- configureMCMC(model)
  configured$resetMonitors()
  configured$setSamplers(c("delta", "delta1", "delta2", "mu"))
  configured$addMonitors(c("delta","delta1", "delta2", "mu"))
  configured$setThin(10)

  ## Build and compile
  built <- buildMCMC(configured)
  compiled <- compileNimble( model, built, showCompilerOutput = TRUE )

  ## Run sampler
  compiled$built$run(niter = 5000)

  ## Save samples
  samples <- as.matrix(compiled$built$mvSamples)
  saveRDS(samples, paste( "./results/samples/bird", toString(x), ".rds", sep=""))
  ##sink()

  
  print(paste("Simulation", toString(x), "complete.", sep = " "))
  print("-------------------------------------------")
}

## RUN SIMULATION
for ( i in 1:NBIRDS ){
  runMCMC(i)
}
