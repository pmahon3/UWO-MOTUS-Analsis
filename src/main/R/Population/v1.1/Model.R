modelCode <- nimbleCode(
  {
    #LIKELIHOODS
    for ( i in 1:nBirds ){
      for ( j in 1:nDays ){
        for ( k in 1:nObservations){
          ## Identify period of day
          p[i,j,k] <- step( t[i,j,k] - delta[1,i,j] ) +
            step( t[i,j,k] - delta[2,i,j] - kappa[i] * step(i - nDays)) + 1
          ## Model response
          y[i,j,k] ~ dnorm( muY[i,j,p[i,j,k]],tauY[i,j,p[i,j,k]])
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
      kappa[i] ~ dnorm( muKappa, 1 / sigmaKappa^2 )
    }
   
    ## HYPERPRIORS
    for ( i in 1:nBirds){
      for(k in 1:2){
        etaDelta[k,i] ~ dnorm(muDelta[k], 1/ sigmaDelta[k]^2)
        xiDelta[k,i] ~ T(dt(0, tau_xiDelta, df_xiDelta),0,Inf)
        tauDelta[k,i] <- 1/xiDelta[k]^2 
      }
    }
    
    muKappa ~ dnorm( muMuKappa, 1 / sigmaMuKappa^2 )
    
    ## GLOBAL PRIORS
    for(k in 1:3){
      ## HYPERPRIORS
      mu_y[k] ~ dnorm(eta_y[k], 1 / sigmaMu[k]^2 )
      xi_y[k] ~ T(dt(0,tau_xi_y, df_xi_y),0,Inf)
      tau_y[k] <- 1/xi_y[k]^2

      
      for ( i in 1:nBirds){
        for(j in 1:nDays){
          
          muY[i,j,k] ~ dnorm( mu_y[k], tau_y[k] )
          xi[i,j,k] ~ T(dt(0,tau_xi, df_xi), 0, )
          
          tauY[i,j,k] <- 1/xi[i,j,k]^2
        }
    }
  }
)
