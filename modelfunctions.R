
HDIofMCMC <- function( sampleVec, credMass ){
  
  sortedPts <- sort( sampleVec )
  ciIdxInc <- floor( credMass * length( sortedPts ))
  nCIs = length( sortedPts ) - ciIdxInc
  for ( i in 1:nCIs ){
    ciWidth[ i ] <- sortedPts[ i + ciIdxInc ] - sortedPts[ i ] 
  }
  
  HDImin <- sortedPts[ which.min( ciWidth )] 
  HDImax <- sortedPts[ which.min( ciWidth ) + ciIdxInc ]
  HDIlim <- c( HDImin, HDImax )
  return(HDIlim)
}