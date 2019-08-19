library(tibble)

#' periodVector returns a vector denoting which period each measurment is in.
#'
#' @param bird A tibble of the time and db measurements for the given bird.
#' @param delta1 The first change point for the given bird.
#' @param delta2 The second change point for the given bird. 
#'
#' @return period is a 1-1 vector mapping onto the entries of the tibble "bird" denoting the period of each observation. 
#' @export
#'
#' @examples

periodVector <- function( bird, delta1, delta2 ){
  
  
  N = nrow( bird )
  period <- vector( mode = "numeric", length = N )
  tdat <- bird$times
  
  for ( i in 1:N ){
    period [i] = floor( tdat[i] / delta1 ) + floor ( tdat[i] / delta2 ) + 1
  }
  
  return( period ) 
}

modelSingleBird <- function( bird, mu1_init, mu2_init, mu3_init, sd_mu1_init, sd_mu2_init, sd_mu3_init, delta1_init, delta2_init, dattrim, ntrim, plot, monitor, monitorVars ){
  load.module("glm")
  ## Initial values and data argument
  
  MU1_INIT <- mu1_init
  MU2_INIT <- mu2_init
  MU3_INIT <- mu3_init
  SD_MU1_INIT <- sd_mu1_init
  SD_MU2_INIT <- sd_mu2_init
  SD_MU3_INIT <- sd_mu3_init
  DELTA1_INIT <- delta1_init
  DELTA2_INIT <- delta2_init
  N <- nrow( BIRD ) 
  YDAT <- BIRD$msrmnts
  TDAT <- BIRD$times
  
  mu_y <- c( MU1_INIT, MU3_INIT, MU3_INIT )
  sds <- c( SD_MU1_INIT, SD_MU2_INIT, SD_MU3_INIT )
  deltas <- c( DELTA1_INIT, DELTA2_INIT )
  
  ## data trimming for speed; plotting
  NSUB <- ntrim
  
  if ( dattrim ){
    
    sub <- ceiling( seq( from = 1, to = N, by = NSUB ))
    N <- N/NSUB
    dat <- list( "y" = YDAT[ sub ], "t" = TDAT[ sub ], "N" = N )
    
  } else {
    
    dat <- list( "y" = YDAT, "t" = TDAT, "N" = N )
    
  }
  
  if ( plot ){
    plot( dat$t, dat$y, type = "l", xlim = c( 0, 24 ) * 3600 )
  }
  
  init = list( tau = 1/sds^2, m_y = mu_y, delta = deltas ) 
  model <- jags.model( "singleBirdModel", data = dat, inits = init, n.chains = 3, n.adapt = 1000 )
  
  if ( monitor )
  monitorVar <- coda.samples( model, variable.names = monitorVars, n.iter = 2000 )
  
  if( plot && monitor ) {
    plot( monitorVar )
  }
  
  return ( model, monitorVar)
}