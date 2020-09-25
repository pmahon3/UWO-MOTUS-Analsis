library(parallel)
library(HDInterval)

NCORES = 6
NBIRDS = 100

## PARALLEL FUNCTION DEFINITION
runMCMC <- function(x) {
  print(paste("Simulation", toString(x), sep = " "))
  print("------------------------------------------")
  ##messagelog <- file(paste("messages", toString(x), ".txt", sep = ""), open = "wt")
  ##outputlog <- file(paste("output", toString(x), ".txt", sep = "" ), open = "wt")
  ##sink(file = messagelog, type = "message")
  ##sink(file = outputlog, type = "output")
  library(nimble)
  dataAndConstants <- dataSimulation(x, constants, trueParams)
  ##saveRDS(dataAndConstants[["constants"]], paste("./results/data/paramsBird", toString(x), ".rds", sep = ""))
  model <- nimbleModel( code = modelCode, name = "model", constants = dataAndConstants[["constants"]], data = dataAndConstants[["data"]], calculate = FALSE)
  configured <- configureMCMC(model)
  configured$resetMonitors()
  configured$setSamplers(c("delta", "delta1", "delta2", "mu"))
  configured$addMonitors(c("delta","delta1", "delta2", "mu"))
  configured$setThin(10)
  built <- buildMCMC(configured)
  compiled <- compileNimble( model, built, showCompilerOutput = TRUE )
  compiled$built$run(niter = 5000)
  samples <- as.matrix(compiled$built$mvSamples)
  saveRDS(samples, paste( "./results/samples/bird", toString(x), ".rds", sep=""))
  ##sink()
  print(paste("Simulation", toString(x), "complete.", sep = " "))
  print("-------------------------------------------")
}

source("Inputs.R")
source("DataSimulation.R")
## RUN SIMULATION
for ( i in 1:NBIRDS ){
  runMCMC(i)
}