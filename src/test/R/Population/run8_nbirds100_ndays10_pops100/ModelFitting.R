library(parallel)
library(HDInterval)

NCORES = 6
NPOPS = 100

## PARALLEL FUNCTION DEFINITION
runMCMC <- function(x, model) {
  messagelog <- file(paste("./results/logs/messages/messages", toString(x), ".txt", sep = ""), open = "wt")
  outputlog <- file(paste("./results/logs/output/output", toString(x), ".txt", sep = "" ), open = "wt")
  sink(file = messagelog, type = "message")
  sink(file = outputlog, type = "output")
  
  library(nimble)
  
  dataAndConstants <- dataSimulation(x = x, CONSTANTS = constants, TRUEPARAMS = trueParams, saveDat = TRUE)
  model$setData(dataAndConstants[["constants"]])
  configured <- configureMCMC(model)
  configured$resetMonitors()
  configured$setSamplers(c("mu_delta_prime", "delta_prime[1:100]", "mu_delta[1:2]", "delta[1:100,1:2]"))
  configured$addMonitors(c("delta","delta_prime", "mu_delta_prime"))
  configured$setThin(10)
  built <- buildMCMC(configured)
  compiled <- compileNimble( model, built)
  
  compiled$built$run(niter = 5000)
  samples <- as.matrix(compiled$built$mvSamples)
  saveRDS(samples, paste( "./results/samples/population", toString(x), ".rds", sep=""))
  
  sink()
}

source("Inputs.R")
source("DataSimulation.R")
initSim <- dataSimulation(x = 1, CONSTANTS = constants, TRUEPARAMS = trueParams, saveDat = FALSE)
t <- initSim$constants$t
model <- nimbleModel( code = modelCode, name = "model", constants = initSim[["constants"]], calculate = FALSE )

## RUN SIMULATION
mclapply(seq(1, NPOPS),  FUN = runMCMC, model = model, mc.cores = NCORES, mc.set.seed = TRUE, mc.silent = TRUE, mc.cleanup = TRUE)
