library(parallel)
library(simfunctions)

#!/usr/bin/Rscript

PATH <- "Out"
# generate a parameter matrix
NPOPS <- 20
NCHAINS <- 3
NBIRDS <- 10
N <- 48
TSPAN <- 24
TSTEP <- TSPAN / N
NDAYS <- 5

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
SD_MU_DELTA1 <- 1
SD_MU_DELTA2 <- 1
SD_MU_MU_DELTA1 <- 1
SD_MU_MU_DELTA2 <- 1
MU_MU_MU_DELTA1 <- 6
MU_MU_MU_DELTA2 <- 20

DELTA_PRIME <- 0.25
SIGMA_EPSILON <- 1/30
SIGMA_DELTA_PRIME <- 0.5

dir.create(file.path( PATH, "Data" ), recursive = TRUE )
dir.create(file.path( PATH, "HPD" ), recursive = TRUE )

pars_mat <- sim_pars_mat( nDays = NDAYS, nPops = NPOPS, nBirds = NBIRDS, tStep = TSTEP, tSpan = TSPAN, mu_mu1 = MU_MU1, mu_mu2 = MU_MU2 , mu_mu3 = MU_MU3, sd_mu_mu1 = SD_MU_MU1, sd_mu_mu2 = SD_MU_MU2, sd_mu_mu3 = SD_MU_MU3, mu_sd1 = MU_SD1, mu_sd2 = MU_SD2, mu_sd3 = MU_SD3 , sd_mu_sd1 = SD_MU_SD1 , sd_mu_sd2 = SD_MU_SD2, sd_mu_sd3 = SD_MU_SD3, mu_delta1 = MU_DELTA1, mu_delta2 = MU_DELTA2, mu_mu_mu_delta1 = MU_MU_MU_DELTA1, mu_mu_mu_delta2 = MU_MU_MU_DELTA2, sd_mu_mu_delta1 = SD_MU_MU_DELTA2, sd_mu_mu_delta2 = SD_MU_MU_DELTA2, sd_mu_delta1 = SD_MU_DELTA1, sd_mu_delta2 = SD_MU_DELTA2, sd_delta1 = SD_DELTA1, sd_delta2 = SD_DELTA2, delta_prime = DELTA_PRIME, sigma_epsilon = SIGMA_EPSILON, sigma_delta_prime = SIGMA_DELTA_PRIME)

print("Making cluster...")
cl <- makeCluster(getOption("cl.cores", 6))
print("Loading cluster dependencies...")
clusterEvalQ(cl, c(library(rjags), library(dplyr), library(simfunctions)))
print("Exporting cluster...")
clusterExport(cl, c("pars_mat", "sim_function"))
print("Computing cluster...")
out <- clusterApply(cl, fun = sim_function, 1:NPOPS, pars_mat)

source("DP_HPDAnalysis.R")