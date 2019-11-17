library(dplyr)
## NEED TO INCLUDE TIBBLE PACKAGE IN SIFUNCTIONS
## CHANGE ARGS FOR simPopulationParams IN SIMFUNCTIONS
library(simfunctions)
library(rjags)

## SIMMING A POPULATION OF BIRDS

NBIRDS <- 100
TSTEP <- 0.005
TSPAN <- 24
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

MU_DELTA1 <- 6
MU_DELTA2 <- 20
SD_MU_DELTA1 <- 1
SD_MU_DELTA2 <- 1


birdPop <- simPopulationParams( nBirds = NBIRDS, mu_mu1 = MU_MU1, mu_mu2 = MU_MU2, mu_mu3 = MU_MU3, sd_mu_mu1 = SD_MU_MU1, sd_mu_mu2 = SD_MU_MU2, sd_mu_mu3 = SD_MU_MU3, mu_sd1 = MU_SD1, mu_sd2 = MU_SD2, mu_sd3 = MU_SD3, sd_mu_sd1 = SD_MU_MU1, sd_mu_sd2 = SD_MU_MU2, sd_mu_sd3 = SD_MU_MU3, mu_delta1 = MU_DELTA1, mu_delta2 = MU_DELTA2, sd_delta1 = SD_MU_DELTA1, sd_delta2 = SD_MU_DELTA2)
birdDat <- simPopulationData( birdPop, tStep = TSTEP, tSpan = TSPAN )

## FITTING A POPULATION OF BIRDS 
load.module("glm")
# Initial values and data args
mu_mu_delta <- vector(mode = "integer", length = 3)
sd_mu_delta <- vector(mode = "integer", length = 3)
mu_mu <- vector(mode = "integer", length = 3)
sd_mu <- vector(mode = "integer", length = 3)

mu_sd_delta <- vector(mode = "integer", length = 2)
sd_sd_delta <- vector(mode = "integer", length = 2)

mu_mu[1] <- -59
mu_mu[2] <- -20
mu_mu[3] <- -59
sd_mu[1] <- 5
sd_mu[2] <- 5
sd_mu[3] <- 5

mu_mu_delta[1] <- 5.5
mu_mu_delta[2] <- 19.5
sd_mu_delta[1] <- 1
sd_mu_delta[2] <- 1

mu_sd_delta[1] <- 1
mu_sd_delta[2] <- 1
sd_sd_delta[1] <- 0.01
sd_sd_delta[2] <- 0.01

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
  
  dat <- list( "y" = ysub, "t" = tsub, "n" = N, "nBirds" = NBIRDS, "mu_mu_delta" = mu_mu_delta, "sd_mu_delta" = sd_mu_delta, "mu_sd_delta" = mu_sd_delta, "sd_sd_delta" = sd_sd_delta, "mu_mu" = mu_mu, "sd_mu" = sd_mu)
  
} else{
  dat <- list( "y" = ysub, "t" = t, "n" = N, "nBirds" = NBIRDS, "mu_mu_delta" = mu_mu_delta, "sd_mu_delta" = sd_mu_delta, "mu_sd_delta" = mu_sd_delta, "sd_sd_delta" = sd_sd_delta, "mu_mu" = mu_mu, "sd_mu" = sd_mu )
}

#inits

model <- jags.model( "populationModel.txt", data = dat, n.chains = NCHAINS, n.adapt = NADAPT )
monitor <- coda.samples( model, variable.names = c("mu_delta" ), n.iter = NITER )

summary(monitor)
plot(monitor)