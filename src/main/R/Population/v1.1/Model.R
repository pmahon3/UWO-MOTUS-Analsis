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
        for(p in 1:2){
          delta[p,i,j] ~ dnorm( muDelta[p,i], tauDelta[p,i] )
        }
      }
      kappa[i] ~ dnorm( muKappa, 1 / sigmaKappa^2 )
    }
   
    ## HYPERPRIORS
    for ( i in 1:nBirds){
      for(p in 1:2){
        muDelta[p,i] ~ dnorm(etaDelta[p], 1/ sigmaDelta[p]^2)
        xiDelta[p,i] ~ T(dt(0, tau_xiDelta, df_xiDelta),0,Inf)
        tauDelta[p,i] <- 1/xiDelta[p]^2 
      }
    }
    
    muKappa ~ dnorm( muMuKappa, 1 / sigmaMuKappa^2 )
    
    ## GLOBAL PRIORS
    for(p in 1:3){
      ## HYPERPRIORS
      mu_y[p] ~ dnorm(eta_y[p], 1 / sigmaMu[p]^2 )
      xi_y[p] ~ T(dt(0,tau_xi_y, df_xi_y),0,Inf)
      tau_y[p] <- 1/xi_y[p]^2

      
      for ( i in 1:nBirds){
        for(j in 1:nDays){
          
          muY[i,j,p] ~ dnorm( mu_y[p], tau_y[p] )
          xi[i,j,p] ~ T(dt(0,tau_xi, df_xi), 0, )
          
          tauY[i,j,p] <- 1/xi[i,j,p]^2
        }
    }
  }
)
