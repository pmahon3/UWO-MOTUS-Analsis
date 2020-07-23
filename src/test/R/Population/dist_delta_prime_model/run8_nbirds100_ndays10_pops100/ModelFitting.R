library(parallel)
library(HDInterval)

NCORES = 6
NPOPS = 100

## PARALLEL FUNCTION DEFINITION
runMCMC <- function(x) {
  messagelog <- file(paste("messages", toString(x), ".txt", sep = ""), open = "wt")
  outputlog <- file(paste("output", toString(x), ".txt", sep = "" ), open = "wt")
  sink(file = messagelog, type = "message")
  sink(file = outputlog, type = "output")
  library(nimble)
  dataAndConstants <- dataSimulation(x, constants, trueParams)
  model <- nimbleModel( code = modelCode, name = "model", constants = dataAndConstants[["constants"]], data = dataAndConstants[["data"]], calculate = FALSE)
  configured <- configureMCMC(model)
  configured$resetMonitors()
  configured$setSamplers(c("mu_delta_prime[1:100]", "delta_prime[1:100]", "delta[1:100,1:2]"))
  configured$addMonitors(c("delta","delta_prime", "mu_delta_prime"))
  configured$setThin(10)
  built <- buildMCMC(configured)
  compiled <- compileNimble( model, built, showCompilerOutput = TRUE )
  compiled$built$run(niter = 5000)
  samples <- as.matrix(compiled$built$mvSamples)
  saveRDS(samples, paste( "./results/samples/population", toString(x), ".rds", sep=""))
}

source("Inputs.R")
source("DataSimulation.R")
## RUN SIMULATION
mclapply(seq(1, NPOPS),  runMCMC, mc.cores = NCORES, mc.set.seed = TRUE, mc.silent = TRUE, mc.cleanup = TRUE)
