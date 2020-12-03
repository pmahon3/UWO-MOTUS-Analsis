modelCode <- nimbleCode(
  {
    #LIKELIHOODS
    for ( i in 1:nBirds ){
      for ( j in 1:nDays ){
        for ( k in 1:nObservations){
          ## Identify period of day
          p[i,j,k] <- step( t[i,j,k] - delta[1,i,j] ) +
            step( t[i,j,k] - delta[2,i,j] - delta.prime * step(i - nDays)) + 1
          ## Model response
          y[i,j,k] ~ dnorm( mu[i,p[i,j,k]],tau[p[i,j,k]])
        }
      }
    }
    
    # PRIORS
    for ( i in 1:nBirds){
      for ( j in 1:nDays ){
        for(k in 1:2){
          delta[k,i,j] ~ dnorm( etaDelta[k,i], tauDelta[k,i] )
        }
      }
      delta.prime[i] ~ dnorm( muDelta.prime, 1 / sigmaMuDelta.prime^2 )
    }
   
    ## HYPERPRIORS
    for ( i in 1:nBirds){
      for(k in 1:2){
        etaDelta[k,i] ~ dnorm(muDelta[k], 1/ sigmaDelta[k]^2)
        xiDelta[k,i] ~ T(dt(0, tau_xiDelta, df_xiDelta),0,Inf)
        tauDelta[k,i] <- 1/xiDelta[k]^2 
      }
    }
    
    muDelta.prime ~ dnorm( muMuDelta.prime, sigmaMuMuDelta.prime^2)
    
    ## GLOBAL PRIORS
    for(k in 1:3){
      ## HYPERPRIORS
      mu_y[k] ~ dnorm(eta_y[k], 1 / sigmaMu[k]^2 )
      xi_y[k] ~ T(dt(0,tau_xi_y, df_xi_y),0,Inf)
      tau_y[k] <- 1/xi_y[k]^2
      
      for ( i in 1:nDays){
        mu[i,k] ~ dnorm( mu_y[k], tau_y[k] )
      }
      
      xi[k] ~ T(dt(0,tau_xi, df_xi), 0, )
      tau[k] <- 1/xi[k]^2 
    }
  }
)
