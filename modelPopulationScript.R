library(dplyr)
## NEED TO INCLUDE TIBBLE PACKAGE IN SIFUNCTIONS
## CHANGE ARGS FOR simPopulationParams IN SIMFUNCTIONS
library(simfunctions)
library(rjags)

## SIMMING A POPULATION OF BIRDS

NBIRDS <- 100
TSTEP <- 12
TSPAN <- 86400
N <- TSPAN/TSTEP

MU_MU1 <- -80
MU_MU2 <- -40
MU_MU3 <- -80
SD_MU_MU1 <- 5
SD_MU_MU2 <- 5
SD_MU_MU3 <- 5

MU_SD1 <- 5
MU_SD2 <- 5
MU_SD3 <- 5
SD_MU_SD1 <- .001
SD_MU_SD2 <- .001
SD_MU_SD3 <- .001

MU_DELTA1 <- 0.2 * 86400
MU_DELTA2 <- 0.75 * 86400
SD_MU_DELTA1 <- 60
SD_MU_DELTA2 <- 60

birdPop <- simPopulationParams( nBirds = NBIRDS, mu_mu1 = MU_MU1, mu_mu2 = MU_MU2, mu_mu3 = MU_MU3, sd_mu_mu1 = SD_MU_MU1, sd_mu_mu2 = SD_MU_MU2, sd_mu_mu3 = SD_MU_MU3, mu_sd1 = MU_SD1, mu_sd2 = MU_SD2, mu_sd3 = MU_SD3, sd_mu_sd1 = SD_MU_MU1, sd_mu_sd2 = SD_MU_MU2, sd_mu_sd3 = SD_MU_MU3, mu_delta1 = MU_DELTA1, mu_delta2 = MU_DELTA2, sd_delta1 = SD_MU_DELTA1, sd_delta2 = SD_MU_DELTA2)
birdDat <- simPopulationData( birdPop, tStep = TSTEP, tSpan = TSPAN )

## FITTING A POPULATION OF BIRDS 
load.module("glm")
# Initial values and data args

MU_MU1_INIT <- -59
MU_MU2_INIT <- -20
MU_MU3_INIT <- -59
SD_MU1_INIT <- 5
SD_MU_MU2_INIT <- 5
SD_MU_MU3_INIT <- 5

MU_SD1_INIT <- 5
MU_SD2_INIT <- 5
MU_SD3_INIT <- 5
SD_MU_SD1_INIT <- .001
SD_MU_SD2_INIT <- .001
SD_MU_SD3_INIT <- .001

MU_DELTA1_INIT <- 0.2 * 86400
MU_DELTA2_INIT <- 0.7 * 86400
SD_MU_DELTA1_INIT <- 60
SD_MU_DELTA2_INIT <- 60
NCHAINS <- 3
NADAPT <- 1000
NITER <- 2000


# Formatting data for model 

y <- array( dim = c( NBIRDS, N))
t <- array( dim = c( NBIRDS, N))

for ( i in 1:NBIRDS ){
  y[i, ] <- birdDat[[i]]$msrmnts
  t[i, ] <- birdDat[[i]]$times      
}

# data trimming for speed 
DATTRIM <- TRUE
NSUB <- 100
PLOT <- TRUE

if ( DATTRIM ){

  sub <- ceiling( seq( from = 1, to = N, by = NSUB ))
  N <- N/NSUB
  ysub <- array( dim = c(NBIRDS, N))
  tsub <- array( dim = c(NBIRDS, N))
  
  for ( i in 1:NBIRDS ){
    ysub[i, ] <- y[i, sub]
    tsub[i, ] <- t[i, sub]
  }
  
  dat <- list( "y" = ysub, "t" = tsub, "n" = N, "nBirds" = NBIRDS)
  
} else{
  dat <- list( "y" = y, "t" = t, "n" = N, "nBirds" = NBIRDS)
}

#inits

model <- jags.model( "populationModel", data = dat, n.chains = NCHAINS, n.adapt = NADAPT )
monitor <- coda.samples( model, variable.names = c("delta[1]", "delta[2]" ), n.iter = NITER )
HPD <- HPDinterval( monitor )

if( PLOT ) {
  
  # average lower and upper bounds for different chains
  deltaHPDbounds = matrix(data = 0, nrow = 2, ncol = 2, dimnames = list( c("delta1", "delta2"), c("lower", "upper")))
  for ( i in 1:length(HPD) ){
 
    deltaHPDbounds["delta1", "lower"] = deltaHPDbounds["delta1", "lower"] + HPD[[i]][1,1]
    deltaHPDbounds["delta1", "upper"] = deltaHPDbounds["delta1", "upper"] + HPD[[i]][1,2]
    deltaHPDbounds["delta2", "lower"] = deltaHPDbounds["delta2", "lower"] + HPD[[i]][2,1]
    deltaHPDbounds["delta2", "upper"] = deltaHPDbounds["delta2", "upper"] + HPD[[i]][2,2]
    
  }
  deltaHPDbounds = deltaHPDbounds/length(HPD)
  plot(monitor)
}  