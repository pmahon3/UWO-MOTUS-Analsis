library(foreach)
library(doParallel)
library(nimble)
library(HDInterval)
library(extraDistr)

##print("Preprocessing data...")
##source("Preprocessing.R")
source("Model.R")

data = readRDS('data.rds')

constants = readRDS('constants.rds')

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
saveRDS(samples, paste( "./samples.rds", sep=""))
