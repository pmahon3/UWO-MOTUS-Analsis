library(foreach)
library(doParallel)
library(nimble)
library(HDInterval)

NCORES = 8
NPOPS = 6

## Parallel function definition
runMCMC <- function(x) {
  print(paste("Simulation", toString(x), sep = " "))
  print("------------------------------------------")
  messagelog <- file(paste("messages", toString(x), ".txt", sep = ""), open = "wt")
  outputlog <- file(paste("output", toString(x), ".txt", sep = "" ), open = "wt")
  sink(file = messagelog, type = "message")
  sink(file = outputlog, type = "output")
  dataAndConstants <- dataSimulation(x, constants, trueParams, TRUE)
  saveRDS(dataAndConstants[["constants"]], paste("./results/data/paramsPopulation", toString(x), ".rds", sep = ""))
  model <- nimbleModel( code = modelCode, name = "model", constants = dataAndConstants[["constants"]], data = dataAndConstants[["data"]], calculate = FALSE)
  configured <- configureMCMC(model)
  configured$resetMonitors()
  configured$setSamplers(c("muDelta.prime", "delta.prime", "muDelta", "delta"))
  configured$addMonitors(c("muDelta.prime","delta.prime", "muDelta", "delta"))
  configured$setThin(10)
  built <- buildMCMC(configured)
  compiled <- compileNimble( model, built, showCompilerOutput = TRUE )
  compiled$built$run(niter = 5000)
  samples <- as.matrix(compiled$built$mvSamples)
  saveRDS(samples, paste( "./results/samples/population", toString(x), ".rds", sep=""))
  sink()
  print(paste("Simulation", toString(x), "complete.", sep = " "))
  print("-------------------------------------------")
}

source("Inputs.R")
source("DataSimulation.R")
source("Model.R")
registerDoParallel(NCORES)

## Run Simulation
foreach ( i=1:NPOPS ) %dopar% {
  runMCMC(i)
}

## Plot results
for (i in 1:NPOPS){
  samplesFile = paste("./results/samples/population", toString(i), ".rds", sep="")
  samples = data.frame(readRDS(samplesFile))
  plot(seq(1,length(samples$muDelta.prime)), samples$muDelta.prime, type = 'l')
}
