modelCode <- nimbleCode(
  {
    #LIKELIHOODS
    for ( i in 1:nDays ){
      for ( j in 1:nObservations){
        ## Identify period of day
        k[i,j] <- step( t[i,j] - delta[1,i] ) +
          step( t[i,j] - delta[2,i] - delta.prime * step(i - nDays)) + 1

        ## Model response
        y[i,j] ~ dnorm( mu[i,k[i,j]],tau[k[i,j]])
      }
    }

    # PRIORS
    for ( i in 1:nDays ){
      for(k in 1:2){
        delta[k,i] ~ dnorm( etaDelta[k], tauDelta[k] )
      }
    }
    delta.prime ~ dnorm( muDelta.prime, 1 / sigmaMuDelta.prime^2 )

    ## HYPERPRIORS
    for(k in 1:2){
      etaDelta[k] ~ dnorm(muDelta[k], 1/ sigmaDelta[k]^2)
      xiDelta[k] ~ T(dt(0, 50, 4),0,Inf)
      tauDelta[k] <- 1/xiDelta[k]^2 
    }
    
    ## GLOBAL PRIORS
    for(k in 1:3){
      ## HYPERPRIORS
      mu_y[k] ~ dnorm(eta_y[k], 1 / sigmaMu[k]^2 )
      xi_y[k] ~ T(dt(0,5.5e-3,4),0,Inf)
      tau_y[k] <- 1/xi_y[k]^2

      for ( i in 1:nDays){
        mu[i,k] ~ dnorm( mu_y[k], tau_y[k] )
      }

      xi[k] ~ T(dt(0,5.5e-3, 4), 0, )
      tau[k] <- 1/xi[k]^2 
    }
  }
)
