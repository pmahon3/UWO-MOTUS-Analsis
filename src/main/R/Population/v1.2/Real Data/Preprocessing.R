library(mclust)
library(plyr)
library(tidyverse)
library(nimble)
library(chron)

# read in data
dat <- data.frame(readRDS("quiescence.rds"))

#### Data Selection ####
morbey_radio = data.frame(read.csv("morbey_radio.csv"))

dat <-  dat %>%
  # Extract time values
  mutate(ts = as.chron(as.POSIXct(ts, format="%Y-%m-%d %H:%M:%OS"))) %>% 
  mutate(secs = hours(ts)*3600 + minutes(ts)*60 + seconds(ts)) %>% 
  # Set day
  mutate(day = dates(ts)) %>%
  # Filter out a window
  filter( between(secs, 4*3600, 8*3600) | between(secs, 17*3600, 21*3600)) %>%  
  # Determine if it is the last day
  mutate(last_day = (dates(as.chron(depart_night2))==dates(ts)) * 1) %>% 
  # Set bird index
  mutate(modelId = as.integer(as.factor(motusTagID))) %>%
  # Get number of days
  group_by(modelId) %>%
  mutate(nDay = length(unique(dates(ts)))) %>%
  ungroup() %>%
  # Get the first day
  group_by(modelId) %>%
  # Determine day number 
  mutate(modelDay = day-min(day) + 1) %>%
  ungroup() %>%
  # Convert seconds to hours
  mutate(t = secs/(60*60))

## Keep individuals captured more than once
dat <- dat %>%
    filter(nDay > 1) %>%
    ## Take subset of data for testing
    head(100000)

## Order observations by ID, year, day, and time for convenience
dat <- dat %>%
  arrange(modelId,year,modelDay,t)

## Count number of observed days per bird
days <- dat %>%
  select(modelId, nDay) %>%
  distinct() %>%
  arrange(modelId) #%>% select(nDay)

# Select relevant variables
dat <- dat %>%
  select(motusTagID, modelId, modelDay, last_day, t, sig)

## Compute basic statistis
nObs <- nrow(dat)
nBirds <- length(unique(dat$modelId))

#### Hyperparameter Fitting ####

## Signal strength ##
# fit mixture model to two components of signal strength
GMM <- Mclust(dat$sig, G=2)
GMMparams <- GMM$parameters

# select hyperparameters from GMM parameters (note: variance from GMM is taken as the hyperparameter for stdev)
etaMuMuY <- c(GMMparams$mean[1], GMMparams$mean[2], GMMparams$mean[1])
thetaMuMuY <- c(GMMparams$variance$sigmasq[1], GMMparams$variance$sigmasq[2], GMMparams$variance$sigmasq[1])

#### Hyperparameter and Constant Selection ####

constants = list(
  ## Design parameters
  nObs = nObs,
  nBirds = nBirds,
  nDays = days$nDay,
  
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
  
  ## 3) Signaldays strength
  
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

saveRDS(list('y' = dat$sig,'t'= dat$t, 'id' = dat$modelId, 'day' = dat$modelDay, 'last_day' = dat$last_day), file='data.rds')
saveRDS(constants, file='constants.rds')
