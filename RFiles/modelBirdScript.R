library(dbplyr)
library(tibble)
## NEED TO INCLUDE TIBBLE PACKAGE IN SIFUNCTIONS
library(simfunctions)
library(modelfunctions)
library(rjags)

## SIMMING 1 BIRD

TSTEP <- 12
TSPAN <- 86400
MU1 <- -58
MU2 <- -34
MU3 <- -58
SD_MU1 <- 5
SD_MU2 <- 5
SD_MU3 <- 5
DELTA1 <- 6.5 * 3600
DELTA2 <- 20.5 * 3600

bird <- simBirdData(tStep = TSTEP, tSpan = TSPAN, mu1 = MU1, mu2 = MU2 , mu3 = MU3 , sd_mu1 = SD_MU1, sd_mu2 = SD_MU2, sd_mu3 = SD_MU3, delta1 = DELTA1, delta2 = DELTA2)
## FITTING 1 BIRD
load.module("glm")
## Initial values and data args

MU1_INIT <- -50
MU2_INIT <- -30
MU3_INIT <- -50
SD_MU1_INIT <- 5
SD_MU2_INIT <- 5
SD_MU3_INIT <- 5
DELTA1_INIT <- TSPAN * .33
DELTA2_INIT <- TSPAN * .66
N <- nrow( bird ) 
YDAT <- bird$msrmnts
TDAT <- bird$times

mu_y <- c( MU1_INIT, MU3_INIT, MU3_INIT )
sds <- c( SD_MU1_INIT, SD_MU2_INIT, SD_MU3_INIT )
deltas <- c( DELTA1_INIT, DELTA2_INIT )

## data trimming for speed 
DATTRIM <- TRUE
NSUB <- 100
PLOT <- TRUE

if ( DATTRIM ){
  
  sub <- ceiling( seq( from = 1, to = N, by = NSUB ))
  N <- N/NSUB
  dat <- list( "y" = YDAT[ sub ], "t" = TDAT[ sub ], "N" = N )
  
} else {
  
  dat <- list( "y" = YDAT, "t" = TDAT, "N" = N )
  
}

if ( PLOT ){
  plot( dat$t, dat$y, type = "l", xlim = c( 0, 24 ) * 3600 )
}

init = list( tau = 1/sds^2, m_y = mu_y, delta = deltas ) 
model <- jags.model( "singleBirdModel", data = dat, inits = init, n.chains = 3, n.adapt = 1000)
deltaMonitor <- coda.samples( model, variable.names = c("delta"), n.iter = 2000  )

if( PLOT ) {
  plot( deltaMonitor )
}