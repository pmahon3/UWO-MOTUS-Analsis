library(HDInterval)

coverage <- array( dim = c(NPOPS + 1, 1) )
muDeltaPrimeCoverage = 0

for ( i in 1: NPOPS ){

	## INDIVIDUAL DELTA_PRIME COVERAGE
  simulatedParams <- readRDS(paste("./results/SimulatedParams", toString(i), ".rds",
sep = ""))
  fittedParams <- hdi(readRDS(paste( "./results/Samples", toString(i), ".rds", sep="")))
  individualDeltaPrimeCoverage = 0

  for ( j in 1:CONSTANTS$nBirds ){
		bird = paste("delta_prime[", toString(j), "]", sep="")
  	deltaPrimeLower <- fittedParams[1, bird]
		deltaPrimeUpper <- fittedParams[2, bird]
		deltaPrime <- simulatedParams[j,1]

		if ( deltaPrime >= deltaPrimeLower && deltaPrime <= deltaPrimeUpper ){
   		individualDeltaPrimeCoverage = individualDeltaPrimeCoverage + 1
		}
  }

  ## MU_DELTA_PRIME COVERAGE
  muDeltaPrimeLower = fittedParams[1, "mu_delta_prime"]
  muDeltaPrimeUpper = fittedParams[2, "mu_delta_prime"]

  if ( muDeltaPrimeLower <= muDeltaPrime && muDeltaPrimeUpper >= muDeltaPrime ){
     muDeltaPrimeCoverage = muDeltaPrimeCoverage + 1
  }

  individualDeltaPrimeCoverage = individualDeltaPrimeCoverage / CONSTANTS$nBirds
	coverage[i] = individualDeltaPrimeCoverage
}

overallCoverage = muDeltaPrimeCoverage / NPOPS
coverage[NPOPS + 1] = overallCoverage

saveRDS(coverage, "Coverage.rds")
