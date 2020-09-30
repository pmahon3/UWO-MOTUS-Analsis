dataSimulation <- function(x, CONSTANTS, TRUEPARAMS){
  ## Initialize data list
  data = list(size = CONSTANTS$nDays)

  ## Initialize storage matrices
  ydat = matrix( nrow = CONSTANTS$nDays, ncol = CONSTANTS$nObservations)
  tdat = matrix( nrow = CONSTANTS$nDays, ncol = CONSTANTS$nObservations)
  deltas = matrix( nrow = CONSTANTS$nDays, ncol = 2)

  ## Loop over days
  for ( day in 1:CONSTANTS$nDays ){
    ## Simulate changepoints
    delta1 = rnorm( 1, TRUEPARAMS$muDelta1, TRUEPARAMS$sigmaDelta1)
    delta2 = rnorm( 1, TRUEPARAMS$muDelta2, TRUEPARAMS$sigmaDelta2)

    ## Define observation times
    tdat[day,] = c(
      seq(from = delta1 - 1, to = delta1 + 1, length.out = CONSTANTS$nObservations/2),
      seq(from = delta2 - 1, to = delta2 + 1, length.out = CONSTANTS$nObservations/2)
    )

    ## Loop over observations within each day
    for ( j in 1:CONSTANTS$nObservations){
      ## Identify parameters of signal strength
      if ( tdat[day,j] < delta1 ){
        # Period 1
        mu = TRUEPARAMS$muY[1]
        sd = TRUEPARAMS$sigmaY[1]
      }
      else if ( day == TRUEPARAMS$nDays && tdat[day,j] < delta2 + TRUEPARAMS$delta ){
        ## Period 2 of final day
        mu = TRUEPARAMS$muY[2]
        sd = TRUEPARAMS$sigmaY[2]
      }
      else if( tdat[day,j] < delta2 ){
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

    ## Store deltas
    deltas[day, 1] = delta1
    deltas[day, 2] = delta2
  }

  ## Return output
  DATA = c( list(y = ydat, ds = deltas) )
  CONSTANTS = c(CONSTANTS, list(t = tdat))
  return(list(data = DATA,constants = CONSTANTS))
}
