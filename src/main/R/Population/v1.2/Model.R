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

    for ( i in 1:nBirds ){
      for ( j in 1:nDays ){
        for(p in 1:2){
          delta[i,j,p] ~ dnorm( muDelta[i,p], tauDelta[i,p] )
        }

        for ( k in 1:nObservations){
          ## Identify period of day
          p[i,j,k] <- step( t[i,j,k] - delta[i,j,1] ) +
            step( t[i,j,k] - delta[i,j,2]) + 1
          ## Model response
          y[i,j,k] ~ dnorm( muY[i,j,p[i,j,k]],tauY[i,j,p[i,j,k]])
        }
      }
      
      delta.prime[i] <- mean(delta[i,1:(nDays-1),2]) - delta[i,nDays,2]
    }
    
    

    for(p in 1:2){
      muMuDelta[p] ~ dnorm(etaMuDelta[p], 1/ thetaMuDelta[p]^2)

      for ( i in 1:nBirds){
        muDelta[i,p] ~ dnorm(muMuDelta[p], 1/ sigmaMuDelta[p]^2)
        sigmaDelta[i,p] ~ T(dt(0, sSigmaDelta, dfSigmaDelta),0,Inf)
        tauDelta[i,p] <- 1/xiDelta[p]^2
      }
    }

    for(p in 1:3){
      muMuY[p] ~ dnorm(etaY[p], 1 / sigmaEtaY[p]^2 )
      sigmaMuY[p] ~ T(dt(0,sSigmaMuY, dfSigmaMuY), 0, Inf)
      tauMuY[p] <- 1/sigmaMuY[p]^2

      for ( i in 1:nBirds){
        ## Bird to bird variability 
        for(j in 1:nDays){
          muY[i,j,p] ~ dnorm( muMuY[p], tauMuY[p] )
          sigmaY[i,j,p] ~ T(dt(0, sSigmaY, dfSigmaY), 0, Inf)
          tauY[i,j,p] <- 1/sigmaY[i,j,p]^2
        }
      }
    }
  }
)
