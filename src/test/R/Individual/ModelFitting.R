cat("\014")
library(parallel)
NCORES = 4


runMCMC <- function(x){
  library(nimble)
  
  source("DataSimulation.R", local = TRUE)
  
  mcmc.out <- nimbleMCMC(
    code = nimCode, 
    constants = CONSTANTS, 
    data = DATA, 
    nchains = 3, 
    niter = 5000,
    summary = TRUE
  )
  
  return(mcmc.out)
}

seed = set.seed(Sys.time())
cluster <- makeCluster(NCORES)

output <- parLapply(cl = cluster, X=1:NCORES, fun = runMCMC)

stopCluster(cluster)

for ( i in 1:NCORES ){
  print(output[[i]]$summary$all.chains)
}