library(foreach)
library(doParallel)
library(nimble)
library(HDInterval)
library(extraDistr)

NCORES = 5
NPOPS = 25

## Parallel function definition
runMCMC <- function(x) {
  # Output logging
#  messagelog <- file(paste("./results/messages/messages", toString(x), ".txt", sep = ""), open = "wt")
#  outputlog <- file(paste("./results/output/output", toString(x), ".txt", sep = "" ), open = "wt")

#  sink(messagelog, type = "message")
#  sink(outputlog, type = "output") 
  # Simulation
  print(paste("Simulation", toString(x), sep = " "))
  print("------------------------------------------")
  data <- dataSimulation(x, constants, trueParams, TRUE)
  saveRDS(constants, paste("./results/data/populationInputs", toString(x), ".rds", sep = ""))
  model <- nimbleModel(code = modelCode,
                       name = "model",
                       constants = constants,
                       data = data,
                       calculate = FALSE)
  configured <- configureMCMC(model)
  configured$resetMonitors()
  configured$setThin(10)  
  configured$addMonitors(c("muDelta.prime",
                           "sigmaDelta.prime",
                           "delta.prime",
                           "muMuDelta",
                           "muDelta",
                           "delta",
                           "sigmaDelta",
                           "muMuY",
                           "sigmaMuY",
                           "muY",
                           "muSigmaY",
                           "sigmaSigmaY",
                           "sigmaY"))
  built <- buildMCMC(configured)
  compiled <- compileNimble( model, built, showCompilerOutput = TRUE)
  compiled$built$run(niter = 3000)
  
  # Save results
  samples <- as.matrix(compiled$built$mvSamples)
  saveRDS(samples, paste( "./results/samples/population", toString(x), ".rds", sep=""))
  print(paste("Simulation", toString(x), "complete.", sep = " "))
  print("-------------------------------------------")
#  sink(NULL)
#  sink(NULL) 
}

source("Inputs.R")
source("Simulation.R")
source("Model.R")

#registerDoParallel(NCORES)

## Run in parallel
for ( i in 1:NPOPS ) {
  runMCMC(i)
}
