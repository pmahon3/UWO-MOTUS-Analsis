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
      seq(from = CONSTANTS$window1[1],
          to = CONSTANTS$window1[2],
          length.out = CONSTANTS$nObservations/2),
      seq(from = CONSTANTS$window2[1],
          to = CONSTANTS$window2[2],
          length.out = CONSTANTS$nObservations/2)
    )

    ## Loop over observations within each day
    period <- (tdat[day,] > deltas[day, 1]) +
      (tdat[day,] > (deltas[day,2] + (day == CONSTANTS$nDays) * TRUEPARAMS$delta)) + 1

    ## Simulate signal 
    ydat[day, ] = rnorm(length(tdat[day,]),
                        TRUEPARAMS$muY[day,period],
                        TRUEPARAMS$sigmaY[period])
  }

  ## Return output
  DATA = c( list(y = ydat, ds = deltas) )

  CONSTANTS = c(CONSTANTS, list(t = tdat))
  
  return(list(data = DATA,constants = CONSTANTS))
}
