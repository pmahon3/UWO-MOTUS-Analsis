library(nimble)

avg <- nimbleFunction(
  run = function(summands = double(1), size = double(0) ){
    returnType(double(0))
    tot = 0
    for ( i in 1:size ) {
      tot = tot + summands[i]
    }
    return ( tot / size )
  }
)

nimCode <- nimbleCode(
  
  {
    
    ##Individual level likelihood
    for ( i in 1:nBirds ){
      
      ##Day level likelihood
      for ( j in 1:nDays-1 ){
        
        ##Up until the penultimate day
        for ( k in 1:nObservations ){
   
          y[i,j,k] ~ dnorm( 
            mu[ i, 
                step( t[i,j,k] - delta[i,j,1] ) + step( t[i,j,k] - delta[i,j, 2] ) + 1
            ] , 
            
            tau[ i, 
                 step( t[i,j,k] - delta[i,j,1] ) + step( t[i,j,k] - delta[i,j, 2] ) + 1
            ]
          )
          
        }
      }
      
      
      ##The ultimate day
      for ( k in 1:nObservations ){
             
        y[i,nDays,k] ~ dnorm( 
          mu[ i,
              step( t[i,nDays,k] - delta[i,nDays,1] ) + step( t[i,nDays,k] - delta[i,nDays, 2] - delta_prime[i] ) + 1
          ],
          tau[ i, 
               step( t[i,nDays,k] - delta[i,nDays,1] ) + step( t[i,nDays,k] - delta[i,nDays, 2] - delta_prime[i] ) + 1
          ]
        )
        
      }
    }
    
    ## Individual level priors
    for( i in 1:nBirds ){

      ## Individual level mode priors
      for( j in 1:3 ){
        mu[i, j ] ~ dnorm( mu_mu_y[j], 1 / ( sd_mu_y[j]^2 ) ) 
        tau[ i, j] ~ dgamma( 10, 1/2 )
      }

      ## Individual level, Intraday, changepoint priors
      for( j in 1:nDays ){
        delta[i,j, 1] ~ dnorm( mu_delta[i,1], tau_delta[i,1] ) 
        delta[i,j, 2] ~ dnorm( mu_delta[i,2], tau_delta[i,2] )
      }

      ## Individual level, Interday, changepoint priors

      mu_delta[i,1] ~ dnorm(mu_mu_delta1, tau_mu_delta1)
      mu_delta[i,2] ~ dnorm(mu_mu_delta2, tau_mu_delta2)
      
      tau_delta[i,1] ~ dgamma(1, 1/2)
      tau_delta[i,2]~ dgamma(1, 1/2)

      delta_prime[i] ~ dnorm( mu_delta_prime, 1 )

        
    }

    ##Population changepoint priors

    avg_delta_prime <- avg( delta_prime[1:nBirds], nBirds ) 
    
    mu_mu_delta1 ~ dnorm(mu_mu_mu_delta1, tau_mu_mu_delta1)
    mu_mu_delta2 ~ dnorm(mu_mu_mu_delta2, tau_mu_mu_delta2)
    
    tau_mu_mu_delta1 ~ dgamma(1,1/2)
    tau_mu_mu_delta2 ~ dgamma(1,1/2)
    
    tau_mu_delta1 ~ dgamma(1,1/2)
    tau_mu_delta2 ~ dgamma(1,1/2)
  }
)


DELTA_PRIME = 1/4

CONSTANTS = list(
  
  nDays = 15,
  

  nBirds = 50,
  
  nObservations = 96,
  
  interval = 24, 


  ## TOP LEVEL HYPER PARAMETERS
  
  mu_mu_y = c(  one = -80, two = -50,  three = -80  ),
  
  sd_mu_y =  c(   one = 1,   two =  1,  three =  1  ),
  
  mu_mu_delta1 = 6, 
  
  mu_mu_delta2 = 18,
  
  
  # Change Point Shift 

  mu_delta_prime = 3/8,
  
  sigma_delta_prime = 1/120
)

