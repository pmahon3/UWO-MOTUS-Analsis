library(parallel)
library(HDInterval)

NCORES = 6
NBIRDS = 100

## PARALLEL FUNCTION DEFINITION
runMCMC <- function(x) {
  messagelog <- file(paste("./results/logs/messages/messages", toString(x), ".txt", sep = ""), open = "wt")
  outputlog <- file(paste("./results/logs/output/output", toString(x), ".txt", sep = "" ), open = "wt")
  sink(file = messagelog, type = "message")
  sink(file = outputlog, type = "output")
  library(nimble)
  dataAndConstants <- dataSimulation(x, constants, trueParams)
  saveRDS(dataAndConstants[["constants"]], paste("./results/data/paramsBird", toString(x), ".rds", sep = ""))
  model <- nimbleModel( code = modelCode, name = "model", constants = dataAndConstants[["constants"]], data = dataAndConstants[["data"]], calculate = FALSE)
  configured <- configureMCMC(model)
  configured$resetMonitors()
  configured$setSamplers(c("delta", "delta1", "delta2"))
  configured$addMonitors(c("delta","delta1", "delta2"))
  configured$setThin(10)
  built <- buildMCMC(configured)
  compiled <- compileNimble( model, built)
  compiled$built$run(niter = 5000)
  samples <- as.matrix(compiled$built$mvSamples)
  saveRDS(samples, paste( "./results/samples/bird", toString(x), ".rds", sep=""))
}

source("Inputs.R")
source("DataSimulation.R")
## RUN SIMULATION
mclapply(seq(1, NBIRDS),  runMCMC, mc.cores = NCORES, mc.set.seed = TRUE)
