library(mclust)
library(plyr)
library(tidyverse)
library(nimble)

# read in data
dat <- data.frame(readRDS("quiescence.rds"))

# extract times and dates
dat <-  dat %>% separate(ts, c('day', 'time'), sep=" ", remove=TRUE) %>% 
  separate(time, c("hours", "minutes", "seconds"), sep=":") %>% 
  mutate(hours = as.numeric(hours)) %>% 
  mutate(minutes = as.numeric(minutes)) %>%
  mutate(seconds = as.numeric(seconds)) %>%
  mutate(t = hours*60*60 + minutes*60 + seconds)

#### Hyperparameter Fitting ####

## Signal strength ##
# fit mixture model to two components of signal strength
GMM <- Mclust(dat$sig, G=2)
GMMparams <- GMM$parameters

# select hyperparameters from GMM parameters (note: variance from GMM is taken as the hyperparameter for stdev)
etaMuMuY <- c(GMMparams$mean[1], GMMparams$mean[2], GMMparams$mean[1])
thetaMuMuY <- c(GMMparams$variance$sigmasq[1], GMMparams$variance$sigmasq[2], GMMparams$variance$sigmasq[1])

#### Data Selection ####
birds <- unique(dat %>% select(motusTagID))
birds <- birds$motusTagID
nBirds <- length(birds)
nDays <- list()
nObs <- list()
y <- list()
t <- list()
maxDays = 0;
maxObs = 0;

for (i in 1:nBirds) {
  days <- unique(dat %>% filter(motusTagID == birds[i]) %>% select(day))
  days <- days$day
  nDays[i] <- length(days)
  
  if (maxDays < nDays[[i]]) maxDays = nDays[[i]]
  
  y[[i]] <- list()
  t[[i]] <- list()
  bird_nObs <- list()
  for (j in 1:nDays[[i]]){
    obs <- dat %>% filter(motusTagID == birds[i], day == days[j]) %>% select(sig, t)
    bird_nObs[j] = length(obs$t)
    
    if (maxObs < bird_nObs[j]) maxObs = bird_nObs[[j]]
    
    y[[i]][[j]] <- obs$sig 
    t[[i]][[j]] <- obs$t
  }
  nObs[[i]] = bird_nObs
}

y_mat = array(dim = c(nBirds,maxDays,maxObs))
t_mat = array(dim = c(nBirds,maxDays,maxObs))
nDays = as.vector(nDays, mode="numeric")
nObs_mat = matrix(nrow=nBirds,ncol=maxObs)
for (i in 1:nBirds){
  for(j in 1:nDays[i]){
    nObs_mat[i,j] = nObs[[i]][[j]]
    for (k in 1:nObs[[i]][[j]]){
      y_mat[i,j,k] = y[[i]][[j]][[k]]
      t_mat[i,j,k] = t[[i]][[j]][[k]]
    }
  }
}

y = y_mat
t = t_mat
nObs = nObs_mat

#### Hyperparameter and Constant Selection ####

constants = list(
  ## Design parameters
  nBirds = nBirds,
  nDays = nDays,
  nObservations = nObs,
  
  ## Hyperparameters
  
  ## 1) Changepoints
  
  ## Overall mean
  etaMuMuDelta = c(6, 18),
  thetaMuMuDelta = c(1,1),
  
  ## Variation between birds
  sSigmaMuDelta = c(1,1,1),
  dfSigmaMuDelta = c(5,5,5),
  
  ## Variation within birds
  sSigmaDelta = c(1.0, 1.0, 1.0),
  dfSigmaDelta = c(5.0, 5.0, 5.0), 
  
  ## 2) Final day effect
  
  ## Means
  etaMuDelta.prime = 0.25,
  thetaMuDelta.prime = 2,
  
  ## Variation between birds
  sSigmaDelta.prime = 1,
  dfSigmaDelta.prime = 5,
  
  ## 3) Signal strength
  
  ## Overall means by period
  etaMuMuY = etaMuMuY,
  thetaMuMuY = thetaMuMuY,
  
  ## Variation between birds
  sSigmaMuY = c(.1,.1,.1),
  dfSigmaMuY = c(5,5,5),
  
  ## Variation within birds
  sSigmaSigmaY = c(.1,.1,.1),
  dfSigmaSigmaY = c(5,5,5),
  
  etaSigmaY = c(1,1,1),
  thetaSigmaY = c(5,5,5)
)


#### Save ####

saveRDS(list('y'=y,'t'=t), file='data.rds')
saveRDS(constants, file='constants.rds')
