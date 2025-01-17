library(foreach)
library(doParallel)
library(nimble)
library(HDInterval)

NCORES = 8
NPOPS = 1

## Parallel function definition
runMCMC <- function(x) {
  # Output logging
  messagelog <- file(paste("./results/messages/messages", toString(x), ".txt", sep = ""), open = "wt")
  outputlog <- file(paste("./results/output/output", toString(x), ".txt", sep = "" ), open = "wt")
  #sink(file = messagelog, type = "message")
  #sink(file = outputlog, type = "output")
  
  # Simulation
  print(paste("Simulation", toString(x), sep = " "))
  print("------------------------------------------")
  dataAndConstants <- dataSimulation(x, constants, trueParams, TRUE)
  saveRDS(dataAndConstants[["constants"]], paste("./results/data/populationInputs", toString(x), ".rds", sep = ""))
  model <- nimbleModel( code = modelCode, name = "model", constants = dataAndConstants[["constants"]], data = dataAndConstants[["data"]], calculate = FALSE)
  configured <- configureMCMC(model)
  configured$resetMonitors()
  configured$setSamplers(c("delta.prime", "muMuDelta", "muDelta", "delta"))
  configured$addMonitors(c("delta.prime", "muMuDelta", "muDelta", "delta"))
  configured$setThin(10)
  built <- buildMCMC(configured)
  compiled <- compileNimble( model, built, showCompilerOutput = TRUE )
  compiled$built$run(niter = 5000)
  
  # Save results
  samples <- as.matrix(compiled$built$mvSamples)
  saveRDS(samples, paste( "./results/samples/population", toString(x), ".rds", sep=""))
  #sink()
  print(paste("Simulation", toString(x), "complete.", sep = " "))
  print("-------------------------------------------")
}

source("Inputs.R")
source("DataSimulation.R")
source("Model.R")
registerDoParallel(NCORES)

## Run Simulation
#foreach ( i=1:NPOPS ) %dopar% {
#  runMCMC(i)
#}

## Plot results
for (i in 1:NPOPS){
  runMCMC(i)
  samplesFile = paste("./results/samples/population", toString(i), ".rds", sep="")
  samples = data.frame(readRDS(samplesFile))
  plot(seq(1,length(samples$muDelta.prime)), samples$muDelta.prime, type = 'l')
}
