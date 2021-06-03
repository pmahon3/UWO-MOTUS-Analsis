library(nimble)

nBirds = 20
nDays = 10
nObservations = 600

trueParams = list(
  nBirds = nBirds,
  nDays = nDays,
  nObservations = nObservations,
  muMuY = c(-80,-50, -80),
  sigmaMuY = c(5, 10, 5),
  dfSdY = c(10,10,10),
  scaleSdY = c(2,3,2),
  
  muMuDelta1 = 6,
  muMuDelta2 = 18,
  sigmaMuDelta1 = .1,
  sigmaMuDelta2 = .1,
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
  sSigmaMuY = 1,
  dfSigmaMuY = 5,

  sSigmaY = 1.0,
  dfSigmaY = 1.0
)
