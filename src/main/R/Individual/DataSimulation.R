source("Inputs.R")

data = list(size = CONSTANTS$nDays)


ydat = matrix( nrow = CONSTANTS$nObservations, ncol = CONSTANTS$nDays)

tdat = c(
  seq(from = CONSTANTS$delta1Mu - 1, to = CONSTANTS$delta1Mu + 1, length.out = CONSTANTS$nObservations/2),
  seq(from = CONSTANTS$delta2Mu - 1, to = CONSTANTS$delta2Mu + 1, length.out = CONSTANTS$nObservations/2)
)


for ( day in 1:CONSTANTS$nDays ){

  for ( i in 1:CONSTANTS$nObservations){
    if ( tdat[i] < CONSTANTS$delta1Mu ){
      mu = CONSTANTS$m_y[1]
      sd = CONSTANTS$sigmaMu[1]
    }
    else if ( day == CONSTANTS$nDays && tdat[i] < CONSTANTS$delta2Mu + DELTA ){
      mu = CONSTANTS$m_y[2]
      sd = CONSTANTS$sigmaMu[2]
    }
    else if( tdat[i] < CONSTANTS$delta2Mu ){
      mu = CONSTANTS$m_y[2]
      sd = CONSTANTS$sigmaMu[2]
    }
    else{
      mu = CONSTANTS$m_y[3]
      sd = CONSTANTS$sigmaMu[3]
    }
    ydat[i,day] = rnorm(1, mu, sd)
  }
}

DATA = c( list(y = ydat))
CONSTANTS = c(CONSTANTS, list(t = tdat))