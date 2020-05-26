cat("\014")
library(parallel)
source("DataSimulation.R")

seed = set.seed(Sys.time())

out <- nimbleMCMC(
  code = nimCode, 
  constants = CONSTANTS, 
  data = DATA, 
  nchains = 3, 
  niter = 5000, 
  summary = TRUE, 
  WAIC = TRUE
)

samples <- out$summary$all.chains["deltaMu"]

print(samples)