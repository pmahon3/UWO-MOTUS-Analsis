library(parallel)
library(HDInterval)

NCORES = 6
NPOPS = 1

## PARALLEL FUNCTION DEFINITION
runMCMC <- function(x) {
  print(paste("Simulation", toString(x), sep = " "))
  print("------------------------------------------")
  messagelog <- file(paste("messages", toString(x), ".txt", sep = ""), open = "wt")
  outputlog <- file(paste("output", toString(x), ".txt", sep = "" ), open = "wt")
  sink(file = messagelog, type = "message")
  sink(file = outputlog, type = "output")
  library(nimble)
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
## RUN SIMULATION
for ( i in 1:NPOPS ){
  runMCMC(i)
}

dat = data.frame(readRDS("./results/samples/population1.rds"))
params = data.frame(readRDS("./results/data/simulatedParams1.rds"))
plot(seq(1,length(dat$muDelta.prime)), dat$muDelta.prime, type = 'l')
