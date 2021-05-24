library(nimble)

nBirds = 20
nDays = 10
nObservations = 600

trueParams = list(
  nDays = nDays,
  nObservations = nObservations,
  muMuY = cbind(rnorm(nDays, -80, 5),
               rnorm(nDays, -50, 5),
               rnorm(nDays, -80, 5)),
  sigmaMuY = c(5, 10, 5),
  shapeSdY = c(1/2,1,1/2),
  scaleSdY = c(1,2,1),
  muMuDelta1 = 6,
  muMuDelta2 = 18,
  sigmaMuDelta1 = .1,
  sigmaMuDelta2 = .1,
  
  muDelta.prime = 1/4,
  sigmaDelta.prime = .05
)

constants = list(
  nBirds = nBirds,
  nDays = nDays,
  nObservations = 600,

  window1 = c(5,7),
  window2 = c(17,19),

  muMuY = c(  one = -80, two = -50,  three = -80  ),
  sigmaMuY = c(   one = 10,   two =  10,  three =  10  ),
  
  etaMuDelta = c(6, 18),
  thetaMuDelta = c(.25, .25),
  sigmaMuDelta = c(2,2),

  sSigmaDelta = 1.0,
  dfSigmaDelta = 5.0,
  xiDelta = c(0.25, 0.25),
  
  etaMuDelta.prime = 0.25,
  thetaMuDelta.prime = 0.5,
  sigmaDelta.prime = 0.25,

  etaY = c(-50,-80,-50),
  sigmaEtaY = c(15,20,15),
  sSigmaMuY = 1,
  dfSigmaMuY = 5,

  sSigmaY = 1.0,
  dfSigmaY = 1.0
)
