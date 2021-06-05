library(nimble)
## Naming Conventions
##
## Indexing:
## i -- individual birds
## j -- day
## k -- observation within day
## p -- period
##
## Unknown parameter values:
## muXxx -- mean of xxx. This may be compound (e.g., muMuX is the mean of muX).
## sigmaXxx -- standard deviation of xxx. This may be compound (e.g., sigmaMuX is the SD of muX).
## tauXxx -- precision of xxx. This is always the square of the inverse of sigmaXxx.
##
## Fixed parameter values: (hyperparameters)
## etaXxx -- mean of xxx. This will be compound (e.g., etaMuX is the mean of muX when this is value is fixed).
## dfXxx -- degrees of freedom when xxx is a standard deviation assigned a half-t prior.
## sXxx -- scale when xxx is a standard deviation assigned a half-t prior.
## thetaXxx -- standard deviation of xxx. 

modelCode <- nimbleCode(
  {
    # Likelihood
    for ( i in 1:nBirds ){
      for ( j in 1:nDays ){
        for ( k in 1:nObservations){
          ## Identify period of day
          p[i,j,k] <- step( t[i,j,k] - delta[i,j,1] ) +
            step( t[i,j,k] - delta[i,j,2] - delta.prime[i] * step(i - nDays)) + 1
          ## Model response
          y[i,j,k] ~ dnorm( muY[i,j,p[i,j,k]],tauY[i,j,p[i,j,k]])
        }
      }
    }

    # delta.prime and muDeltaPrime priors
    for ( bird in 1:nBirds){
      delta.prime[bird] ~ dnorm( muDelta.prime, 1/sigmaDelta.prime^2 )
    }
    muDelta.prime ~ dnorm( etaMuDelta.prime, 1/thetaMuDelta.prime^2 )
    
    # delta, muDelta, and muMuDelta priors
    for(p in 1:2){
      muMuDelta[p] ~ dnorm(etaMuMuDelta[p], 1/thetaMuMuDelta[p]^2)
      for ( bird in 1:nBirds){
        muDelta[bird,p] ~ dnorm(muMuDelta[p], 1/thetaMuDelta[p]^2)
        sigmaDelta[bird,p] ~ T(dt(0, sSigmaDelta, dfSigmaDelta),0,Inf)
        tauDelta[bird,p] <- 1/sigmaDelta[bird,p]
        for ( day in 1:nDays ){
          delta[bird,day,p] ~ dnorm( muDelta[bird,p], tauDelta[bird,p] )
        }
      }
    }
    
    # muY, muMuY, sigmaY, and sigmaMuY priors
    for(p in 1:3){
      muMuMuY[p] ~ dnorm(etaY[p], 1 / sigmaEtaY[p]^2 )
      sigmaMuMuY[p] ~ T(dt(0,sSigmaMuMuY[p], dfSigmaMuMuY[p]), 0, Inf)
      tauMuMuY[p] <- 1/sigmaMuMuY[p]^2
      for (bird in 1:nBirds){
        muMuY[bird,p] ~ dnorm(muMuMuY[p], tauMuMuY[p])
        sigmaMuY[bird,p] ~ T(dt(0,sSigmaMuY[p], dfSigmaMuY[p]), 0, Inf)
        tauMuY[bird,p] <- 1/sigmaMuY[bird,p]^2
        for (day in 1:nDays){
          muY[bird,day,p] ~ dnorm( muMuY[bird,p], tauMuY[bird,p] )
          sigmaY[bird,day,p] ~ T(dt(0, sSigmaY[p], dfSigmaY[p]), 0, Inf)
          tauY[bird,day,p] <- 1/sigmaY[bird,day,p]^2
        }
      }
    }
  }
)
