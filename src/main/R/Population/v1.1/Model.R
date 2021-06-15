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
    ##### Likelihood #####

      ## 1) Signal strength

      ## Observed values
    for ( i in 1:nBirds ){
      for ( j in 1:nDays ){
        for ( k in 1:nObservations){
          ## Identify period of day
          p[i,j,k] <- step( t[i,j,k] - delta[i,j,1] ) +
              step( t[i,j,k] - delta[i,j,2] - delta.prime[i] * step(i - nDays)) +
              1
            
          ## Model response
          y[i,j,k] ~ dnorm( muY[i,j,p[i,j,k]],tauY[i,j,p[i,j,k]])
        }
      }
    }

      ## Period effects
      for(p in 1:3){
          for (bird in 1:nBirds){
              for (day in 1:nDays){
                  muY[bird,day,p] ~ dnorm( muMuY[p], tauMuY[p] )
                  log(sigmaY[bird,day,p]) ~ dnorm(muSdY[p], tauSdY[p])
                  tauY[bird, day, p] <- 1/sigmaY[bird, day, p]^2
              }
          }
      }

 
    ## 2) Changepoints
    # Final day effect
    for ( bird in 1:nBirds){
      delta.prime[bird] ~ dnorm( muDelta.prime, 1/sigmaDelta.prime^2 )
    }
    
    # delta, muDelta, and muMuDelta priors
      for ( bird in 1:nBirds){
          for(p in 1:2){
              muDelta[bird,p] ~ dnorm(muMuDelta[p], 1/thetaMuDelta[p]^2)

              for ( day in 1:nDays ){
                  delta[bird,day,p] ~ dnorm( muDelta[bird,p], 1/sigmaDelta[p]^2)
              }
          }
      }
    
   ##### Priors #####

      ## Changepoints
      muDelta.prime ~ dnorm( etaMuDelta.prime, 1/thetaMuDelta.prime^2 )
      sigmaDelta.prime ~ T(dt(0,sSigmaDelta.prime, dfSigmaDelta.prime), 0, Inf)

      for(p in 1:2){
          muMuDelta[p] ~ dnorm(etaMuMuDelta[p], 1/thetaMuMuDelta[p]^2)
          sigmaDelta[p] ~ T(dt(0,sSigmaDelta[p], dfSigmaDelta[p]), 0, Inf)
      }

      ## Signal strength
      for(p in 1:3){
          ## Mean model
          muMuY[p] ~ dnorm(etaMuY[p], 1 / sigmaMuY[p]^2 )
          sigmaMuY[p] ~ T(dt(0,sSigmaMuY[p], dfSigmaMuY[p]), 0, Inf)
          tauMuY[p] <- 1/sigmaMuY[p]^2

          ## SD model
          muSdY[p] ~ dnorm(etaSdY[p], 1/ sigmaSdY[p]^2)
          sigmaSdY[p] ~ T(dt(0,sSigmaSdY[p], dfSigmaSdY[p]), 0, Inf)
          tauSdY[p] <- 1/sigmaSdY[p]^2
      }
  }
)


