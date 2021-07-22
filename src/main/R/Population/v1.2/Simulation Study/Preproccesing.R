library(mclust)
library(dplyr)

# read in data
dat <- data.frame(readRDS("quiescence.rds"))

##### Hyperparameter Selection######

## Signal strength ##
# fit mixture model to two components of signal strength
GMM <- Mclust(dat$sig, G=2)
GMMparams <- GMM$parameters

# felect hyperparameters from GMM parameters (note: variance from GMM is taken as the hyperparameter for stdev)
etaMuMuY <- c(GMMparams$mean[1], GMMparams$mean[2], GMMparams$mean[1])
thetaMuMuY <- c(GMMparams$variance$sigmasq[1], GMMparams$variance$sigmasq[2], GMMparams$variance$sigmasq[1])