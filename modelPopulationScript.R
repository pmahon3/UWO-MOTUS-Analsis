library(dbplyr)
library(tibble)
## NEED TO INCLUDE TIBBLE PACKAGE IN SIFUNCTIONS
## CHANGE ARGS FOR simPopulationParams IN SIMFUNCTIONS
library(simfunctions)
library(modelfunctions)
library(rjags)

## SIMMING A POPULATION OF BIRDS

NBIRDS <- 100
TSTEP <- 12
TSPAN <- 86400

MU_MU1 <- -59
MU_MU2 <- -20
MU_MU3 <- -59
SD_MU_MU1 <- 5
SD_MU_MU2 <- 5
SD_MU_MU3 <- 5

MU_SD1 <- 5
MU_SD2 <- 5
MU_SD3 <- 5
SD_MU_SD1 <- .1
SD_MU_SD2 <- .1
SD_MU_SD3 <- .1

MU_DELTA1 <- 0.2 * 86400
MU_DELTA2 <- 0.7 * 86400
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
SD_MU_SD1_INIT <- .1
SD_MU_SD2_INIT <- .1
SD_MU_SD3_INIT <- .1

MU_DELTA1_INIT <- 0.2 * 86400
MU_DELTA2_INIT <- 0.7 * 86400
SD_MU_DELTA1_INIT <- 60
SD_MU_DELTA2_INIT <- 60

N <- NBIRDS
