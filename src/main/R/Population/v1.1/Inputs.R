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
  sigmaDelta1 = .1,
  sigmaDelta2 = .1,
  
  delta = 1/4
)

constants = list(
  nDays = nDays,
  window1 = c(5,7),
  window2 = c(17,19),
  nObservations = 600,
  eta_y = c(  one = -80, two = -50,  three = -80  ),
  sigmaMu = c(   one = 10,   two =  10,  three =  10  ),
  
  muDelta = c(6, 18),
  sigmaDelta = c(.25, .25),
  
  muDelta.prime = 0.25,
  sigmaMuDelta.prime = 0.5
)
