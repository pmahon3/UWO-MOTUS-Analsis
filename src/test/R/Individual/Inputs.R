library(nimble)
nimCode <- nimbleCode(
  {
    
    ##likelihood
    for ( j in 1:nDays-1 ){
      for ( i in 1:nObservations){
          y[i,j] ~ dnorm( 
            mu[ step( t[i] - delta1 ) + step( t[i] - delta2 ) + 1 ] , 
            tau[ step( t[i] - delta1 ) + step( t[i] - delta2 ) + 1 ] )
      }
    }
    
    for ( i in 1:nObservations){
        
      y[i,nDays] ~ dnorm( 
        mu[ step( t[i] - delta1 ) + step( t[i] - delta2 - delta) + 1 ] , 
        tau[ step( t[i] - delta1 ) + step( t[i] - delta2 - delta) + 1 ] )
    }
    
    
    ##priors
    for(k in 1:3){
      mu[k] ~ dnorm( m_y[k], 1 )
    }
    
    for(k in 1:3){
      tau[k] ~ dgamma(10, 2)
    }
    
    delta1 ~ dnorm( delta1Mu, 1 ) 
    delta2 ~ dnorm( delta2Mu, 1 ) 
    
    delta ~ dnorm( deltaMu, 1 )
  }
)


DELTA = 1/4

CONSTANTS = list(
  
  nDays = 10,
  
  nObservations = 512,
  
  interval = 24, 
  
  ##HYPER PARAMETERS
  
  m_y     = c(  one = -80, two = -50,  three = -80  ),
  
  sigmaMu = c(   one = 5,   two =  10,  three =  5  ),
  
  delta1Mu = 6, 
  
  delta2Mu = 18, 
  
  sigmaDelta1 = 1, 
  
  sigmaDelta2 = 1, 
  
  # Changepoint Shift
  
  deltaMu = 3/8, 
  
  sigmaDeltaMu = 1/120
)