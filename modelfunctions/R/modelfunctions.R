
HDIofMCMC <- function( sampleVec, credMass ){

  sortedPts <- sort( sampleVec )
  cilIdxInc <- floor( credMass * length( sortedPts ))
  nCIs = length( sortedPts ) - cilIdxInc
  for ( i in 1:nCIs ){
    ciWidth[ i ] <- sortedPts[ i + cilIdxInc ] - sortedPts[ i ]
  }

  HDImin <- sortedPts[ which.min( ciWidth )]
  HDImax <- sortedPts[ which.min( ciWidth ) + cilIdxInc ]
  HDIlim <- c( HDImin, HDImax )
  return(HDIlim)
}
