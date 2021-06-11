## Load packages
# library(foreach) 
#library(doParallel)
library(nimble)
library(HDInterval)
# library(extraDistr)

## Load inputs
source("Inputs.R")

## Source simulation code
source("Simulation.R")

## Simulate data
dataAndConstants <- dataSimulation(-1, constants, trueParams, FALSE)

## Initialized nimble model
model <- nimbleModel( code = modelCode,
                     name = "model",
                     constants = dataAndConstants[["constants"]],
                     data = dataAndConstants[["data"]],
                     calculate = FALSE)

## Configure model
configured <- configureMCMC(model)
configured$resetMonitors()
configured$setThin(10)  

## Set samplers (Is this necessary?)
configured$setSamplers(c("muDelta.prime",
                         "delta.prime",
                         "muMuDelta",
                         "muDelta",
                         "delta",
                         "tauDelta",
                         "muMuMuY",
                         "muMuY",
                         "muY",
                         "sigmaMuMuY",
                         "sigmaMuY",
                         "sigmaY"))

## Add monitors
configured$addMonitors(c("muDelta.prime", "delta.prime", "muMuDelta", "muDelta", "delta", "tauDelta", "muMuMuY", "muMuY", "muY", "sigmaMuMuY", "sigmaMuY", "sigmaY"))

## Build model
built <- buildMCMC(configured)

## Compile model
compiled <- compileNimble( model, built, showCompilerOutput = TRUE )

## Run model
compiled$built$run(niter = 5000)
  
## Examine results
samples <- as.matrix(compiled$built$mvSamples)

