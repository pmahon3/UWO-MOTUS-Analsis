
library(dplyr)

#' simBird simulates the data for one bird
#'
#' @param days is the number of days to simulate
#' @param delta1 first change point
#' @param delta2 second change point
#' @param mu1 first mean signal strength
#' @param mu2 second mean signal strength
#' @param mu3 third mean signal strength
#' @param sd_mu1 sd of first mode
#' @param sd_mu2 sd of second mode
#' @param sd_mu3 sd of third mode
#' @param tStep time between measurements
#' @param tSpan total time measurements taken over
#'
#' @return list of tibbles containg the data for each day
#' @export
#'
#' @examples
simBirdData <- function( nDays, tStep, tSpan, mu1, mu2, mu3, sd_mu1, sd_mu2, sd_mu3, mu_delta1, mu_delta2, sd_delta1, sd_delta2, delta){

  data <- list(length = nDays)

  for ( j in 1:nDays){
    times <- seq( from = tStep, to = tSpan, by = tStep )
    msrmnts <- vector( mode = "double", length = tSpan / tStep )
    delta1 <- rnorm(1, mu_delta1, sd_delta1)
    delta2 <- rnorm(1, mu_delta2, sd_delta2)
    if ( j == nDays ){
       for ( i in 1:length(times) ){
      	  if ( times[i] < delta1 ){
             mean = mu1
             sd = sd_mu1
          }
      	  else if ( times[i] >= delta1 && times[i] < delta2 + delta ){
             mean = mu2
             sd = sd_mu2
          }
          else{
             mean = mu3
             sd = sd_mu3
          }
          msrmnts[i] <- rnorm(1, mean, sd)
      }
      data[[j]] <- cbind(times, msrmnts)
    }
    else{
      for ( i in 1:length(times) ){
        if ( times[i] < delta1 ){
          mean = mu1
          sd = sd_mu1
  	    }
        else if ( times[i] >= delta1 && times[i] < delta2 ){
          mean = mu2
          sd = sd_mu2
        }
        else{
		      mean = mu3
          sd = sd_mu3
        }
        msrmnts[i] <- rnorm(1, mean, sd)
      }
    }
    data[[j]] <- cbind(times, msrmnts)
  }
  return(data)
}

#' simBirdParams simulates the parameters for a population of n birds
#'
#' @param nBirds number of birds to simulate
#' @param tStep time between measurements
#' @param tSpan total time measurements taken over
#' @param mu_mu1 mean of first mode mean signal strength
#' @param mu_mu2 mean of second mode mean signal strength
#' @param mu_mu3 mean of third mode mean signal strength
#' @param sd_mu_mu1 sd of mean of first mode mean signal strength
#' @param sd_mu_mu2 sd of mean of second mode mean signal strength
#' @param sd_mu_mu3 sd of mean of third mode mean signal strength
#' @param mu_sd1 mean of first mode sd
#' @param mu_sd2 mean of second mode sd
#' @param mu_sd3 mean of third mode sd
#' @param sd_mu_sd1 sd of first mode sd
#' @param sd_mu_sd2 sd of second mode sd
#' @param sd_mu_sd3 sd of third mode sd
#' @param mu_delta1 mean of first change point
#' @param mu_delta2 mean of second change point
#' @param sd_delta1 sd of first change point
#' @param sd_delta2 sd of second change point
#' @param delta_prime is the shift of the penultimate day prior to migration
#' @param sigma_epsilon is the standard deviation for noise injection into delta_prime
#'
#' @return
#' @export
#'
#' @examples


simBirdParams <- function( nBirds, mu_mu1, mu_mu2, mu_mu3, sd_mu_mu1, sd_mu_mu2, sd_mu_mu3, mu_sd1, mu_sd2, mu_sd3, sd_mu_sd1, sd_mu_sd2, sd_mu_sd3, mu_mu_delta1, mu_mu_delta2, sd_mu_delta1, sd_mu_delta2, mu_sd_delta1, sd_sd_delta1, mu_sd_delta2, sd_sd_delta2, delta_prime, sigma_epsilon){

    #List of params for nBirds

    mu1 <- rnorm( nBirds, mu_mu1, sd_mu_mu1)
    mu2 <- rnorm( nBirds, mu_mu2, sd_mu_mu2)
    mu3 <- rnorm( nBirds, mu_mu3, sd_mu_mu3)

    sd_mu1 = vector( mode = "double", length = nBirds )
    sd_mu2 = vector( mode = "double", length = nBirds )
    sd_mu3 = vector( mode = "double", length = nBirds )

    delta = vector( mode = "double", length = nBirds )

    for ( i in 1:nBirds ){

      sd_mu1[i] <- rnorm(1,  mu_sd1, sd_mu_sd1)
      sd_mu2[i] <- rnorm(1, mu_sd2, sd_mu_sd2)
      sd_mu3[i] <- rnorm(1,  mu_sd3, sd_mu_sd3)

      delta[i] <- delta_prime + rnorm(1, 0, sigma_epsilon )

      while ( sd_mu1[i] <= 0 ) {
        sd_mu1[i] <- rnorm(1,  mu_sd1, sd_mu_sd1)
      }
      while ( sd_mu2[i] <= 0 ) {
        sd_mu2[i] <- rnorm(1, mu_sd2, sd_mu_sd2)
      }
      while ( sd_mu3[i] <= 0 ) {
        sd_mu3[i] <- rnorm(1, mu_sd3, sd_mu_sd3)
      }
    }

    sd_delta1 <- rnorm( nBirds, mu_sd_delta1, sd_sd_delta1)
    sd_delta2 <- rnorm( nBirds, mu_sd_delta2, sd_sd_delta2)

    mu_delta1 <- rnorm( nBirds, mu_mu_delta1, sd_mu_delta1 )
    mu_delta2 <- rnorm( nBirds, mu_mu_delta2, sd_mu_delta2 )

    birds = data.frame(mu1, mu2, mu3, sd_mu1, sd_mu2, sd_mu3, mu_delta1, mu_delta2, sd_delta1, sd_delta2, delta)
    return(birds)
}


#' simPopulationData simulates the data for a population of birds given their individual parameters
#'
#' @param birds the data frame containing the birds parameters
#' @param tStep the time between measurements
#' @param tSpan the total time the measurements are taken over
#'
#' @return a list of lists where each element is a tibble of time-measurments for each bird
#' @export
#'
#' @examples
simPopulationData <- function( birds, nDays, tStep, tSpan ){
  n <- nrow( birds )
  dat <- vector( mode = "list", length = n)

  for ( row in 1:n ){
      newBird <- simBirdData( nDays = nDays, tStep = tStep , tSpan = tSpan , mu1 = birds[row, "mu1"], mu2 = birds[row, "mu2"], mu3 = birds[row, "mu3"], sd_mu1 = birds[row, "sd_mu1"], sd_mu2 = birds[row, "sd_mu2"], sd_mu3 = birds[row, "sd_mu3"], mu_delta1 = birds[row, "mu_delta1"], mu_delta2 = birds[row, "mu_delta2"], sd_delta1 = birds[row, "sd_delta1"], sd_delta2 = birds[row, "sd_delta2"], delta = birds[row, "delta"])
      dat[[ row ]] <- newBird
    }

    return( dat )
}

#' sim_function conducts simulations for parallel processing
#'
#' @param i is the number of simulations to run (i.e. nrows of pars_mat)
#' @param pars_mat is the matrix of parameters for the various simulations
#'
#' @return NONE SPECIFIED YET
#' @export
#'
#' @examples
sim_function <- function( i, pars_mat){

  path = "Out"
  print(paste("Simulation", i, "...", sep = " "))
  pm <- pars_mat

  nDays = pm[i, "nDays"]
  nBirds = pm[i, "nBirds"]
  mu_mu1 = pm[i, "mu_mu1"]
  mu_mu2 = pm[i, "mu_mu2"]
  mu_mu3 = pm[i, "mu_mu3"]
  sd_mu1 = pm[i, "sd_mu_mu1"]
  sd_mu2 = pm[i, "sd_mu_mu2"]
  sd_mu3 = pm[i, "sd_mu_mu3"]
  mu_sd1 = pm[i, "mu_sd1"]
  mu_sd2 = pm[i, "mu_sd2"]
  mu_sd3 = pm[i, "mu_sd3"]
  sd_mu_sd1 = pm[i, "sd_mu_sd1"]
  sd_mu_sd2 = pm[i, "sd_mu_sd2"]
  sd_mu_sd3 = pm[i, "sd_mu_sd3"]
  mu_mu_delta1 = pm[i, "mu_mu_delta1"]
  mu_mu_delta2 = pm[i, "mu_mu_delta2"]
  sd_mu_delta1 = pm[i, "sd_mu_delta1"]
  sd_mu_delta2 = pm[i, "sd_mu_delta2"]
  mu_sd_delta1 = pm[i, "mu_sd_delta1"]
  sd_sd_delta1 = pm[i, "sd_sd_delta1"]
  mu_sd_delta2 = pm[i, "mu_sd_delta2"]
  sd_sd_delta2 = pm[i, "sd_sd_delta2"]
  mu_mu_mu_delta1 =  pm[i, "mu_mu_mu_delta1"]
  mu_mu_mu_delta2 =  pm[i, "mu_mu_mu_delta2"]
  tStep = pm[i, "tStep"]
  tSpan = pm[i, "tSpan"]
  nObs = tSpan / tStep
  delta_prime = pm[i, "delta_prime"]
  sigma_delta_prime = pm[i, "sigma_delta_prime"]
  sigma_epsilon = pm[i, "sigma_epsilon"]

  mu_mu <- c(mu_mu1, mu_mu2, mu_mu3)
  mu_mu_delta <- c( mu_mu_delta1, mu_mu_delta2)
  sd_mu_delta <- c( sd_mu_delta1, sd_mu_delta2)
  sd_mu <- c(sd_mu1, sd_mu2, sd_mu3)
  mu_mu_mu_delta <- c(mu_mu_mu_delta1, mu_mu_mu_delta2)

  # SLIGHT BUMP ( +4 MINUTES )  TO MU_DELTA_PRIME HYPER PARAMETER FOR TESTING
  mu_delta_prime = delta_prime + 1/15

  birdPop <- simBirdParams( nBirds = nBirds, mu_mu1 = mu_mu1, mu_mu2 = mu_mu2, mu_mu3 = mu_mu3, sd_mu_mu1 = sd_mu1, sd_mu_mu2 = sd_mu2, sd_mu_mu3 = sd_mu3, mu_sd1 = mu_sd1, mu_sd2 = mu_sd2, mu_sd3 = mu_sd3, sd_mu_sd1 = sd_mu_sd1, sd_mu_sd2 = sd_mu_sd2, sd_mu_sd3 = sd_mu_sd3, mu_mu_delta1 = mu_mu_delta1, mu_mu_delta2 = mu_mu_delta2, sd_mu_delta1 = sd_mu_delta1, sd_mu_delta2 = sd_mu_delta2, mu_sd_delta1 = mu_sd_delta1, sd_sd_delta1 = sd_sd_delta1, mu_sd_delta2 = mu_sd_delta2, sd_sd_delta2 = sd_sd_delta2, delta_prime = delta_prime, sigma_epsilon = sigma_epsilon)

  birdDat <- simPopulationData( birdPop, nDays = nDays, tStep = tStep, tSpan = tSpan)

  print(paste("Saving simulation", i, "data", sep = " "))
  saveRDS(birdDat, file = paste( path, "/Data/", "Data", toString(i), ".RDS", sep = ""))

  load.module("glm")



  init_vals <- sim_init_vals(i, mu_mu1, mu_mu2, mu_mu3, sd_mu1, sd_mu2, sd_mu3, delta_prime, sigma_delta_prime)

  tMat <- array( dim = c(nBirds, nDays, tSpan/tStep))
  yMat <- array( dim = c(nBirds, nDays, tSpan/tStep))

  for ( l in 1:nBirds ){
    for ( j in 1:nDays ){
      tMat[l,j,] <- birdDat[[l]][[j]][,1]
      yMat[l,j,] <- birdDat[[l]][[j]][,2]
    }
  }

  delta <- array( dim = c(nBirds, nDays, 2))
  for( l in 1:nBirds ){
       for ( j in 1:nDays ){
       	   delta[l,j,1] = mu_mu_delta1
	         delta[l,j,2] = mu_mu_delta2
       }
  }

  dat <- list( "yMat" = yMat, "tMat" = tMat, "n" = nObs, "nDays" = nDays, "nBirds" = nBirds, "mu_mu_delta" = mu_mu_delta, "mu_mu" = mu_mu, "sd_mu" = sd_mu, "mu_delta_prime" = mu_delta_prime, "sigma_delta_prime" = sigma_delta_prime, "mu_mu_mu_delta" = mu_mu_mu_delta, "delta" = delta)

  print(paste("Building", "model", i, "...", sep = " "))
  model <- jags.model("populationModel.txt", data = dat, n.chains = 3, n.adapt = 1000)

  print(paste("Running chains for model", i, "...", sep = " " ))
  monitor <- coda.samples(model, variable.names = c("delta_prime"), n.iter = 5000)
  summary(monitor)
  saveRDS(monitor, file = paste( path, "/HPD/", "HPD", toString(i), ".RDS", sep = ""))
}

#' sim_init_vals generates initial values for the parallel processed chains
#'
#' @param i is the number of chains being run
#'
#' @return init_vals is a matrix of initial values, rows coressponding to sets of initial values
#' @export
#'
#' @examples
sim_init_vals <- function(i, mu_mu1, mu_mu2, mu_mu3, sd_mu1, sd_mu2, sd_mu3,  mu_delta_prime, sigma_delta_prime){

  init_vals <- vector( mode = "list", length = i)
  for (k in 1:i){

    mu_mu <- vector(mode = "integer", length = 3)
    sd_mu <- vector(mode = "integer", length = 3)

    mu_mu[1] <- mu_mu1
    mu_mu[2] <- mu_mu2
    mu_mu[3] <- mu_mu3

    sd_mu[1] <- sd_mu1
    sd_mu[2] <- sd_mu2
    sd_mu[3] <- sd_mu3

    init_vals[[k]] = list( mu_mu, sd_mu,  mu_delta_prime, sigma_delta_prime)
  }
  return(init_vals)
}


#' sim_pars_mat simulates the parameters for parallel simulations
#'
#' @param nPops number of population to simulate
#' @param nBirds number of birds in a population
#' @param tStep time between observations
#' @param tSpan total time observations taken over
#' @param mu_mu1 mean of first mode mean signal strength
#' @param mu_mu2 mean of second mode mean signal strength
#' @param mu_mu3 mean of third mode mean signal strength
#' @param sd_mu_mu1 sd of mean of first mode mean signal strength
#' @param sd_mu_mu2 sd of mean of second mode mean signal strength
#' @param sd_mu_mu3 sd of mean of third mode mean signal strength
#' @param mu_sd1 mean of first mode sd
#' @param mu_sd2 mean of second mode sd
#' @param mu_sd3 mean of third mode sd
#' @param sd_mu_sd1 sd of first mode sd
#' @param sd_mu_sd2 sd of second mode sd
#' @param sd_mu_sd3 sd of third mode sd
#' @param mu_mu_delta1 population mean of first change point
#' @param mu_mu_delta2 population mean of second change point
#' @param sd_mu_delta1 population sd of first change point
#' @param sd_mu_delta2 population sd of second change point
#' @param mu_mu_mu_delta1 population hyper-parameter for first change point
#' @param mu_mu_mu_delta2 population hyper-parameter for second change point
#' @param delta_prime population
#'
#' @return pm is a matrix of paramter values with each row a specific population
#' @export
#'
#' @examples
sim_pars_mat <- function( nDays, nPops, nBirds, tStep, tSpan, mu_mu1, mu_mu2, mu_mu3, sd_mu_mu1, sd_mu_mu2, sd_mu_mu3, mu_sd1, mu_sd2, mu_sd3, sd_mu_sd1, sd_mu_sd2, sd_mu_sd3, mu_mu_delta1, mu_mu_delta2, sd_mu_delta1, sd_mu_delta2, mu_mu_mu_delta1, mu_mu_mu_delta2, mu_sd_delta1, sd_sd_delta1, mu_sd_delta2, sd_sd_delta2, delta_prime, sigma_delta_prime, sigma_epsilon ){
  pm = matrix( nrow = nPops, ncol = 29, dimnames = list(c(1:nPops), c("nBirds" , "mu_mu1", "mu_mu2", "mu_mu3", "sd_mu_mu1", "sd_mu_mu2", "sd_mu_mu3", "mu_sd1", "mu_sd2", "mu_sd3", "sd_mu_sd1", "sd_mu_sd2", "sd_mu_sd3", "mu_mu_delta1", "mu_mu_delta2", "mu_mu_mu_delta1", "mu_mu_mu_delta2", "sd_mu_delta1", "sd_mu_delta2", "mu_sd_delta1", "sd_sd_delta1", "mu_sd_delta2", "sd_sd_delta2", "tStep", "tSpan", "delta_prime", "sigma_delta_prime", "sigma_epsilon", "nDays")))
  for ( j in 1:nPops ){
    pm[j, "nBirds"] = nBirds
    pm[j, "mu_mu1"] = mu_mu1
    pm[j, "mu_mu2"] = mu_mu2
    pm[j, "mu_mu3"] = mu_mu3
    pm[j, "sd_mu_mu1"] = sd_mu_mu1
    pm[j, "sd_mu_mu2"] = sd_mu_mu2
    pm[j, "sd_mu_mu3"] = sd_mu_mu2
    pm[j, "mu_sd1"] = mu_sd1
    pm[j, "mu_sd2"] = mu_sd2
    pm[j, "mu_sd3"] = mu_sd3
    pm[j, "sd_mu_sd1"] = sd_mu_sd1
    pm[j, "sd_mu_sd2"] = sd_mu_sd2
    pm[j, "sd_mu_sd3"] = sd_mu_sd3


    pm[j, "mu_mu_delta1"] = mu_mu_delta1
    pm[j, "mu_mu_delta2"] = mu_mu_delta2
    pm[j, "mu_sd_delta1"] = mu_sd_delta1
    pm[j, "sd_sd_delta1"] = sd_sd_delta1
    pm[j, "mu_sd_delta2"] = mu_sd_delta2
    pm[j, "sd_sd_delta2"] = sd_sd_delta2
    pm[j, "mu_mu_mu_delta1"] = mu_mu_mu_delta1
    pm[j, "mu_mu_mu_delta2"] = mu_mu_mu_delta2
    pm[j, "sd_mu_delta1"] = sd_mu_delta1
    pm[j, "sd_mu_delta2"] = sd_mu_delta2
    pm[j, "tStep"] = tStep
    pm[j, "tSpan"] = tSpan
    pm[j, "delta_prime"] = delta_prime
    pm[j, "sigma_epsilon"] = sigma_epsilon
    pm[j, "sigma_delta_prime"] = sigma_delta_prime
    pm[j, "nDays"] = nDays
  }
  return(pm)
}
