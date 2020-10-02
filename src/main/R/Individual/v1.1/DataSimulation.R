dataSimulation <- function(x, CONSTANTS, TRUEPARAMS){
  ## Initialize data list
  data = list(size = CONSTANTS$nDays)

  ## Generate parameters
  deltas <- cbind(rnorm( CONSTANTS$nDays, TRUEPARAMS$muDelta1, TRUEPARAMS$sigmaDelta1),
                  rnorm( CONSTANTS$nDays, TRUEPARAMS$muDelta2, TRUEPARAMS$sigmaDelta2))
  
  ## Initialize storage matrices
  ydat = matrix( nrow = CONSTANTS$nDays, ncol = CONSTANTS$nObservations)
  tdat = matrix( nrow = CONSTANTS$nDays, ncol = CONSTANTS$nObservations)

  ## Loop over days
  for ( day in 1:CONSTANTS$nDays ){
    ## Define observation times
    tdat[day,] = c(
      seq(from = deltas[day,1] - 1, to = deltas[day,1] + 1, length.out = CONSTANTS$nObservations/2),
      seq(from = deltas[day,2] - 1, to = deltas[day,2] + 1, length.out = CONSTANTS$nObservations/2)
    )

    ## Loop over observations within each day
    for ( j in 1:CONSTANTS$nObservations){
      ## Identify parameters of signal strength
      if ( tdat[day,j] < deltas[day,1] ){
        # Period 1
        mu = TRUEPARAMS$muY[1]
        sd = TRUEPARAMS$sigmaY[1]
      }
      else if ( day == TRUEPARAMS$nDays && tdat[day,j] < deltas[day,2] + TRUEPARAMS$delta ){
        ## Period 2 of final day
        mu = TRUEPARAMS$muY[2]
        sd = TRUEPARAMS$sigmaY[2]
      }
      else if( tdat[day,j] < deltas[day,2] ){
        ## Period 2 of other days
        mu = TRUEPARAMS$muY[2]
        sd = TRUEPARAMS$sigmaY[2]
      }
      else{
        ## Period 3
        mu = TRUEPARAMS$muY[3]
        sd = TRUEPARAMS$sigmaY[3]
      }

      ## Simulate signal 
      ydat[day, j] = rnorm(1, mu, sd)
    }

  }

  ## Return output
  DATA = c( list(y = ydat, ds = deltas) )
  CONSTANTS = c(CONSTANTS, list(t = tdat))
  return(list(data = DATA,constants = CONSTANTS))
}
