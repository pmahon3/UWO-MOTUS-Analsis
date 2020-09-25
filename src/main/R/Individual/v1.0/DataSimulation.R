dataSimulation <- function(x, CONSTANTS, TRUEPARAMS){
  data = list(size = CONSTANTS$nDays)
  ydat = matrix( nrow = CONSTANTS$nObservations, ncol = CONSTANTS$nDays)
  tdat = c(
    seq(from = TRUEPARAMS$delta1 - 1, to = TRUEPARAMS$delta1 + 1, length.out = CONSTANTS$nObservations/2),
    seq(from = TRUEPARAMS$delta2 - 1, to = TRUEPARAMS$delta2 + 1, length.out = CONSTANTS$nObservations/2)
  )
  for ( day in 1:CONSTANTS$nDays ){
    for ( i in 1:CONSTANTS$nObservations){
      if ( tdat[i] < TRUEPARAMS$delta1 ){
        mu = TRUEPARAMS$muY[1]
        sd = TRUEPARAMS$sigmaY[1]
      }
      else if ( day == TRUEPARAMS$nDays && tdat[i] < TRUEPARAMS$delta2 + TRUEPARAMS$delta ){
        mu = TRUEPARAMS$muY[2]
        sd = TRUEPARAMS$sigmaY[2]
      }
      else if( tdat[i] < TRUEPARAMS$delta2 ){
        mu = TRUEPARAMS$muY[2]
        sd = TRUEPARAMS$sigmaY[2]
      }
      else{
        mu = TRUEPARAMS$muY[3]
        sd = TRUEPARAMS$sigmaY[3]
      }
      ydat[i,day] = rnorm(1, mu, sd)
    }
  }
  DATA = c( list(y = ydat))
  CONSTANTS = c(CONSTANTS, list(t = tdat))
  return(list(data = DATA,constants = CONSTANTS))
}