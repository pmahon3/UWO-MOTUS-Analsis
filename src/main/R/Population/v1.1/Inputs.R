library(nimble)

nBirds = 25
nDays = 15
nObservations = 600

trueParams = list(
  nBirds = nBirds,
  nDays = nDays,
  nObservations = nObservations,
  muMuMuY = c(-80,-50, -80),
  sdMuMuY = c(5, 10, 5),
  dfSdMuY = c(10,10,10),
  scaleSdMuY = c(2,3,2),
  dfSdMuY = c(10,10,10),
  scaleSdMuY = c(2,3,2),
  dfSdY = c(10,10,10),
  scaleSdY = c(2,3,2),
  
  muMuDelta = c(6,18),
  sigmaMuDelta = c(.1,.1),
  dfSigmaDelta = c(10,10),
  scaleSigmaDelta = c(1/10,1/10), 
  
  muDelta.prime = .25,
  sigmaDelta.prime = .05
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
