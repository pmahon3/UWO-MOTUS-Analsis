library(rjags)


for ( i in 1:NPOPS){
    dat <- HPDInterval(readRDS(paste("HPD", i, ".RDS", sep = ""), prob = 0.95 ))
}

