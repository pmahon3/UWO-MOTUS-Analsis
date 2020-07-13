cat("\014")
library(nimble)

for ( i in 1:NPOPS ){
  library(nimble)

  source("DataSimulation.R", local = TRUE)
  saveRDS(simulatedParams, paste("./results/SimulatedParams", toString(i), ".rds", sep = ""))

  print("Running simulation")
  print(i)

  model <- nimbleModel( code = modelCode, name = "model", constants = CONSTANTS, data = DATA)

  compiled <- compileNimble(model, showCompilerOutput = TRUE )
  configured <- configureMCMC( model, print = TRUE )
  configured$addMonitors(c("delta","delta_prime"))
  built <- buildMCMC(configured)
  compiled <- compileNimble( built, project = model, showCompilerOutput = TRUE )

  set.seed(Sys.time())
  compiled$run(5000)

  print("Simulation complete.")

  samples <- as.matrix(compiled$mvSamples)
  saveRDS(samples, paste( "./results/Samples", toString(i), ".rds", sep=""))
}

source("Analysis.R")
