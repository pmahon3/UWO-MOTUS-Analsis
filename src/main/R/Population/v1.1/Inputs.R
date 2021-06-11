library(nimble)

nBirds = 25
nDays = 15
nObservations = 600

trueParams = list(
    ## Basic paramters
  nBirds = nBirds,
  nDays = nDays,
  nObservations = nObservations,

  ## Signal Strength
  muMuY = c(-80,-50, -80), # Mean of means by period
  sdMuY = c(5,10,5),       # Variation of means by period
  muSdY = c(log(5),log(10),log(5)),       # Mean of log-variation by period
  sdSdY = c(.1,.2,.1),        # Variation of log-variation by period

  ## Changepoints
  muMuDelta = c(6,18), # Overall means
  sigmaMuDelta = c(.1,.1), # Individual variation
  sigmaDelta = c(.2,.2),   # Daily variation 

  ## Departure day effect
  muDelta.prime = .25, # Mean
  sigmaDelta.prime = .05 # Individual variation
)

constants = list(
  nBirds = nBirds,
  nDays = nDays,
  nObservations = nObservations,

  window1 = c(5,7),
  window2 = c(17,19),
  
  etaMuMuDelta = c(6, 18),
  thetaMuMuDelta = c(1,1),
  thetaMuDelta = c(1,1),

  sSigmaDelta = 1.0,
  dfSigmaDelta = 5.0,
  xiDelta = c(0.25, 0.25),
  
  etaMuDelta.prime = 0.25,
  thetaMuDelta.prime = 2,
  sigmaDelta.prime = 0.25,

  etaY = c(-80,-50,-80),
  sigmaEtaY = c(15,20,15),
  sSigmaMuMuY = c(1,1,1),
  dfSigmaMuMuY = c(5,5,5),
  sSigmaMuY = c(1,1,1),
  dfSigmaMuY = c(5,5,5),
  sSigmaY = c(1.0,1.0,1.0),
  dfSigmaY = c(1.0,1.0,1.0)
)
