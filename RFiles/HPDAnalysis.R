# HPDAnalysis performs the Highest Posterior Density calculations for the simulated mu_delta parameters

library(rjags)
PATH = "Out/HPD/"

# Matrices containing 0s and 1s, 1 if the parameter is in the 0.95 highest density interval, 0 if not

mu_delta1_results <- matrix( nrow = NPOPS, ncol = NCHAINS )
mu_delta2_results <- matrix( nrow = NPOPS, ncol = NCHAINS ) 

# For each simulation calculate the HPD for each chain and fill in the results matrices
for ( i in 1:NPOPS){

    dat <- HPDinterval(readRDS(paste(PATH, "HPD", i, ".RDS", sep = "")), prob = 0.95 )

    for ( j in 1:NCHAINS ){

    	if ( dat[[j]][1,1] <= MU_DELTA1 && MU_DELTA1 <= dat[[j]][1,2] ){
	     mu_delta1_results[i, j] <- 1
	   }
	else{
	     mu_delta1_results[i, j] <- 0
	}

	if ( dat[[j]][2,1] <= MU_DELTA2 && MU_DELTA2 <= dat[[j]][2,2] ){
	     mu_delta2_results[i, j] <- 1
	}
	else{
	     mu_delta2_results[i, j] <- 0 		 	  
     	}
    }
}

mu_delta1_result <- sum(mu_delta1_results)/(NPOPS*NCHAINS)
mu_delta2_result <- sum(mu_delta2_results)/(NPOPS*NCHAINS)

print(paste("MU_DELTA1:" , mu_delta1_result, sep = " "))
print(paste("MU_DELTA2:", mu_delta2_result, sep = " "))

