library(parallel)
library(HDInterval)

NCORES = 6
NPOPS = 100

## PARALLEL FUNCTION DEFINITION
runMCMC <- function(x) {
   
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
  compiled <- compileNimble( built, project = model, showCompilerOutput = TRUE )

  print(paste("Running simulation ", toString(x), "...", sep = ""))
  set.seed(Sys.time())
  compiled$run(5000)

  print(paste("Simulation ", toString(x), " complete. Saving chains.", sep = ""))

  samples <- as.matrix(compiled$mvSamples)
  saveRDS(samples, paste( "./results/Samples", toString(x), ".rds", sep=""))
}

source("Inputs.R")
source("DataSimulation.R")
## BUILD CLUSTER AND EXPORT INPUTS AND DATA SIMULATION SCRIPTS
cl <- makeCluster(NCORES, outfile = "")
clusterEvalQ(cl, library(nimble))
clusterExport(cl, c("dataSimulation","constants", "trueParams", "modelCode"))
## RUN SIMULATION
parLapply(cl, seq(1, NPOPS),  runMCMC)
stopCluster(cl)