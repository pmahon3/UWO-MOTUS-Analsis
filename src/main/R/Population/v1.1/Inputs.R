library(nimble)

nDays = 10
nBirds = 100

trueParams = list(
  nDays = nDays,
  nObservations = 600,
  muMuY = cbind(rnorm(nDays, -80, 5),
               rnorm(nDays, -50, 5),
               rnorm(nDays, -80, 5)),
  sigmaMuY = c(5, 10, 5),
  muMuDelta1 = 6,
  muMuDelta2 = 18,
  sigmaMuDelta1 = .1,
  sigmaMuDelta2 = .1,
  
  muDelta.prime = 1/4
)

constants = list(
  nDays = nDays,
  window1 = c(5,7),
  window2 = c(17,19),
  nObservations = 600,
  muMuY = c(  one = -80, two = -50,  three = -80  ),
  sigmaMuY = c(   one = 10,   two =  10,  three =  10  ),
  
  etaMuDelta = c(6, 18),
  thetaMuDelta = c(.25, .25),
  
  etaMuDelta.prime = 0.25,
  thetaMuDelta.prime = 0.5
)
