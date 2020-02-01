library(simfunctions)
library(dplyr)
library(rjags)

PATH <- "Out2/"
NPOPS <- 6
NBIRDS <- 6
N <- 24
TSPAN <- 24
TSTEP <- TSPAN/N
NDAYS <- 15
NCHAINS <- 3

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
SD_DELTA1 <- 1
SD_DELTA2 <- 1

DELTA_PRIME <- 0.25
SIGMA_DELTA_PRIME <- 1/30
SIGMA_EPSILON <- 1/30

pars_mat <- sim_pars_mat( nPops = NPOPS, nDays = NDAYS, nBirds = NBIRDS, tStep = TSTEP, tSpan = TSPAN, mu_mu1 = MU_MU1, mu_mu2 = MU_MU2 , mu_mu3 = MU_MU3, sd_mu_mu1 = SD_MU_MU1, sd_mu_mu2 = SD_MU_MU2, sd_mu_mu3 = SD_MU_MU3, mu_sd1 = MU_SD1, mu_sd2 = MU_SD2, mu_sd3 = MU_SD3, sd_mu_sd1 = SD_MU_SD1, sd_mu_sd2 = SD_MU_SD2, sd_mu_sd3 = SD_MU_SD3, mu_delta1 = MU_DELTA1, mu_delta2 = MU_DELTA2, sd_delta1 = SD_DELTA1, sd_delta2 = SD_DELTA2, sigma_delta_prime = SIGMA_DELTA_PRIME, sigma_epsilon = SIGMA_EPSILON, delta_prime = DELTA_PRIME )

for ( i in 1:NPOPS ){
	sim_function(i, pars_mat)
}

source("DP_HPDAnalysis.R")
