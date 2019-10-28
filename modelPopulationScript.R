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

MU_DELTA1 <- 0.25 * 86400
MU_DELTA2 <- 0.75 * 86400
SD_MU_DELTA1 <- 60
SD_MU_DELTA2 <- 60

birdPop <- simPopulationParams( nBirds = NBIRDS, mu_mu1 = MU_MU1, mu_mu2 = MU_MU2, mu_mu3 = MU_MU3, sd_mu_mu1 = SD_MU_MU1, sd_mu_mu2 = SD_MU_MU2, sd_mu_mu3 = SD_MU_MU3, mu_sd1 = MU_SD1, mu_sd2 = MU_SD2, mu_sd3 = MU_SD3, sd_mu_sd1 = SD_MU_MU1, sd_mu_sd2 = SD_MU_MU2, sd_mu_sd3 = SD_MU_MU3, mu_delta1 = MU_DELTA1, mu_delta2 = MU_DELTA2, sd_delta1 = SD_MU_DELTA1, sd_delta2 = SD_MU_DELTA2)
birdDat <- simPopulationData( birdPop, tStep = TSTEP, tSpan = TSPAN )

## FITTING A POPULATION OF BIRDS 
load.module("glm")
# Initial values and data args
mu_mu <- vector(mode = "integer", length = 3)
sd_mu <- vector(mode = "integer", length = 3)

mu_delta <- vector(mode = "integer", length = 2)
sd_delta <- vector(mode = "integer", length = 2)

mu_mu[1] <- -59
mu_mu[2] <- -20
mu_mu[3] <- -59
sd_mu[1] <- 5
sd_mu[2] <- 5
sd_mu[3] <- 5

mu_delta[1] <- 0.2 * 86400
mu_delta[2] <- 0.7 * 86400
sd_delta[1] <- 60
sd_delta[2] <- 60
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
PLOT <- FALSE

if ( DATTRIM ){

  sub <- ceiling( seq( from = 1, to = N, by = NSUB ))
  N <- N/NSUB
  ysub <- array( dim = c(NBIRDS, N))
  tsub <- array( dim = c(NBIRDS, N))
  
  for ( i in 1:NBIRDS ){
    ysub[i, ] <- y[i, sub]
    tsub[i, ] <- t[i, sub]
  }
  
  dat <- list( "y" = ysub, "t" = tsub, "n" = N, "nBirds" = NBIRDS, "mu_mu" = mu_mu, "sd_mu" = sd_mu, "mu_delta" = mu_delta, "sd_delta" = sd_delta)
  
} else{
  dat <- list( "y" = ysub, "t" = t, "n" = N, "nBirds" = NBIRDS, "mu_mu" = mu_mu, "sd_mu" = sd_mu, "mu_delta" = mu_delta, "sd_delta" = sd_delta)
}

#inits

model <- jags.model( "populationModel.txt", data = dat, n.chains = NCHAINS, n.adapt = NADAPT )
monitor <- coda.samples( model, variable.names = c("mean_delta1", "mean_delta2" ), n.iter = NITER )

HPD <- HPDinterval( monitor )

if( PLOT ) {
  
  # average lower and upper bounds for different chains
  deltaHPDbounds = matrix(data = 0, nrow = 2, ncol = 2, dimnames = list( c("mu_delta1", "mu_delta2"), c("lower", "upper")))
  for ( i in 1:length(HPD) ){
 
    deltaHPDbounds["mu_delta1", "lower"] = deltaHPDbounds["mu_delta1", "lower"] + HPD[[i]][1,1]
    deltaHPDbounds["mu_delta1", "upper"] = deltaHPDbounds["mu_delta1", "upper"] + HPD[[i]][1,2]
    deltaHPDbounds["mu_delta2", "lower"] = deltaHPDbounds["mu_delta2", "lower"] + HPD[[i]][2,1]
    deltaHPDbounds["mu_delta2", "upper"] = deltaHPDbounds["mu_delta2", "upper"] + HPD[[i]][2,2]
    
  }
  deltaHPDbounds = deltaHPDbounds/length(HPD)
  plot(monitor)
}  
