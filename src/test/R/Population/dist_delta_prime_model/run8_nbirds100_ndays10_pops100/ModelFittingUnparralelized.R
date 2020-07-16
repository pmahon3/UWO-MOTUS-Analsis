cat("\014")
library(nimble)

source("Inputs.R")
source("DataSimulation.R")

NPOPS = 100

for ( x in 1:NPOPS ){

  dataAndConstants <- dataSimulation(x, constants, trueParams)

  print(paste("Creating model ", toString(x), sep = ""))
  model <- nimbleModel( code = modelCode, name = "model", constants = dataAndConstants[["constants"]], data = dataAndConstants[["data"]], calculate = FALSE)

  print(paste("Configuring model ", toString(x),  sep = ""))
  configured <- configureMCMC(model)
  configured$resetMonitors()
  configured$addMonitors(c("delta","delta_prime", "mu_delta_prime"))
  configured$setThin(10)

  print(paste("Building simulation ", toString(x), sep = ""))
  built <- buildMCMC(configured)

  print(paste("Compiling simulation ", toString(x), "...", sep = ""))
  compiled <- compileNimble( model, built,  showCompilerOutput = TRUE )

  print(paste("Running simulation ", toString(x), "...", sep = ""))
  set.seed(Sys.time())
  compiled$run(5000)

  print(paste("Simulation ", toString(x), " complete. Saving chains.", sep = ""))

  samples <- as.matrix(compiled$mvSamples)
  saveRDS(samples, paste( "./results/Samples", toString(x), ".rds", sep=""))
}