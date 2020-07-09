cat("\014")
library(nimble)

NPOPS = 1

for ( i in 1:NPOPS ){
  library(nimble)

  source("DataSimulation.R", local = TRUE)
  saveRDS(simulated_params, paste("./results/SimulatedParams", toString(i), ".rds", sep = ""))

  print("Running simulation")
  print(i)

  model <- nimbleModel( code = nimCode, name = "model", constants = CONSTANTS )

  compiledModel <- compileNimble(model, showCompilerOutput = TRUE )
  configuredModel <- configureMCMC( model, print = TRUE )
  modelMCMC <- buildMCMC(configuredModel)
  compiledMCMC <- compileNimble( modelMCMC, project = model)
  set.seed(Sys.time())
  compiledMCMC$run(5000)

  print("Simulation complete.")

  samples <- as.matrix(compiledMCMC$mvSamples)
  saveRDS(samples, paste( "./results/Output", toString(x), "Summary.rds", sep=""))
}

source("Analysis.R")