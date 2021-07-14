library(nimble)

nBirds = 25
nDays = 15
nObservations = 60 #600

trueParams = list(
    ## Basic paramters
  nBirds = nBirds,
  nDays = nDays,
  nObservations = nObservations,

  ## Signal Strength
  muMuY = c(-80,-50, -80), # Mean of means by period
  sigmaMuY = c(5,10,5),       # Variation of means by period
  muSigmaY = c(log(5),log(10),log(5)),       # Mean of log-variation by period
  sigmaSigmaY = c(.1,.2,.1),        # Variation of log-variation by period

  ## Changepoints
  muMuDelta = c(6,18), # Overall means
  sigmaMuDelta = c(.1,.1), # Individual variation
  sigmaDelta = c(.2,.2),   # Daily variation 

  ## Departure day effect
  muDelta.prime = .25, # Mean
  sigmaDelta.prime = .05 # Individual variation
)

constants = list(
    ## Design parameters
  nBirds = nBirds,
  nDays = nDays,
  nObservations = nObservations,

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
  etaMuY = c(-80,-50,-80),
  thetaMuMuY = c(15,20,15),

  ## Variation between birds
  sSigmaMuY = c(.1,.1,.1),
  dfSigmaMuY = c(5,5,5),

  ## Variation within birds
  sSigmaSigmaY = c(.1,.1,.1),
  dfSigmaSigmaY = c(5,5,5),

  etaSigmaY = c(1,1,1),
  thetaSigmaY = c(5,5,5)
)
