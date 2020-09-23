dataSimulation <- function(x, CONSTANTS, TRUEPARAMS){
  data = list(size = CONSTANTS$nDays)
  ydat = matrix( nrow = CONSTANTS$nDays, ncol = CONSTANTS$nObservations)
  tdat = matrix( nrow = CONSTANTS$nDays, ncol = CONSTANTS$nObservations)
  deltas = matrix( nrow = CONSTANTS$nDays, ncol = 2)
  for ( day in 1:CONSTANTS$nDays ){
    delta1 = rnorm( 1, TRUEPARAMS$muDelta1, TRUEPARAMS$sigmaDelta1)
    delta2 = rnorm( 1, TRUEPARAMS$muDelta2, TRUEPARAMS$sigmaDelta2)
    tdat[day,] = c(
      seq(from = delta1 - 1, to = delta1 + 1, length.out = CONSTANTS$nObservations/2),
      seq(from = delta2 - 1, to = delta2 + 1, length.out = CONSTANTS$nObservations/2)
    )
    for ( j in 1:CONSTANTS$nObservations){
      if ( tdat[day,j] < delta1 ){
        mu = TRUEPARAMS$muY[1]
        sd = TRUEPARAMS$sigmaY[1]
      }
      else if ( day == TRUEPARAMS$nDays && tdat[day,j] < delta2 + TRUEPARAMS$delta ){
        mu = TRUEPARAMS$muY[2]
        sd = TRUEPARAMS$sigmaY[2]
      }
      else if( tdat[day,j] < delta2 ){
        mu = TRUEPARAMS$muY[2]
        sd = TRUEPARAMS$sigmaY[2]
      }
      else{
        mu = TRUEPARAMS$muY[3]
        sd = TRUEPARAMS$sigmaY[3]
      }
      ydat[day, j] = rnorm(1, mu, sd)
    }
    deltas[day, 1] = delta1
    deltas[day, 2] = delta2
  }
  DATA = c( list(y = ydat, ds = deltas) )
  CONSTANTS = c(CONSTANTS, list(t = tdat))
  return(list(data = DATA,constants = CONSTANTS))
}