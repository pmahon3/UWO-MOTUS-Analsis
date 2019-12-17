library(dplyr)

#' simBird simulates the data for one bird
#'
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
#' @return tibble of times and corresponding measurements
#' @export
#'
#' @examples
simBirdData <- function(  tStep, tSpan, mu1, mu2, mu3, sd_mu1, sd_mu2, sd_mu3, delta1, delta2 ){

  times = seq( from = tStep, to = tSpan, by = tStep)
  msrmnts = vector( mode = "double", length = tSpan/tStep )

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

    while( times[i] <= 0 ){
      msrmnts[i] <- rnorm(1, mean, sd)
    }
  }

  return( tibble( times, msrmnts ) )
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
#'
#' @return
#' @export
#'
#' @examples


simBirdParams <- function( nBirds, mu_mu1, mu_mu2, mu_mu3, sd_mu_mu1, sd_mu_mu2, sd_mu_mu3, mu_sd1, mu_sd2, mu_sd3, sd_mu_sd1, sd_mu_sd2, sd_mu_sd3, mu_delta1, mu_delta2, sd_delta1, sd_delta2 ){

    #List of params for nBirds

    mu1 <- rnorm( nBirds, mu_mu1, sd_mu_mu1)
    mu2 <- rnorm( nBirds, mu_mu2, sd_mu_mu2)
    mu3 <- rnorm( nBirds, mu_mu3, sd_mu_mu3)

    sd_mu1 = vector( mode = "double", length = nBirds )
    sd_mu2 = vector( mode = "double", length = nBirds )
    sd_mu3 = vector( mode = "double", length = nBirds )

    for ( i in 1:nBirds ){

      sd_mu1[i] <- rnorm(1,  mu_sd1, sd_mu_sd1)
      sd_mu2[i] <- rnorm(1, mu_sd2, sd_mu_sd2)
      sd_mu3[i] <- rnorm(1,  mu_sd3, sd_mu_sd3)

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


    delta1 <- rnorm( nBirds, mu_delta1, sd_delta1 )
    delta2 <- rnorm( nBirds, mu_delta2, sd_delta2 )

    birds = data.frame(mu1, mu2, mu3, sd_mu1, sd_mu2, sd_mu3, delta1, delta2)

    # for ( row in 1:nBirds ) {
    #   while ( birds[row, "mu1"] < 0 ){
    #     birds[row, "mu1"] <- rnorm(1, mu_mu1, sd_mu_mu1)
    #   }
    #   while ( birds[row, "mu2"] < 0 ){
    #     birds[row, "mu2"] <- rnorm(1, mu_mu2, sd_mu_mu2)
    #   }
    #   while ( birds[row, "mu3"] < 0 ){
    #     birds[row, "mu3"] <- rnorm(1, mu_mu3, sd_mu_mu3)
    #   }
    # }

    return(birds)

}

simPopulationParams <- function( nBirds, mu_mu1, mu_mu2, mu_mu3, sd_mu_mu1, sd_mu_mu2, sd_mu_mu3, mu_sd1, mu_sd2, mu_sd3, sd_mu_sd1, sd_mu_sd2, sd_mu_sd3, mu_delta1, mu_delta2, sd_delta1, sd_delta2 ){

  #List of params for nBirds

  mu1 <- rnorm( nBirds, mu_mu1, sd_mu_mu1)
  mu2 <- rnorm( nBirds, mu_mu2, sd_mu_mu2)
  mu3 <- rnorm( nBirds, mu_mu3, sd_mu_mu3)

  sd_mu1 = vector( mode = "double", length = nBirds )
  sd_mu2 = vector( mode = "double", length = nBirds )
  sd_mu3 = vector( mode = "double", length = nBirds )

  for ( i in 1:nBirds ){

    sd_mu1[i] <- rnorm(1,  mu_sd1, sd_mu_sd1)
    sd_mu2[i] <- rnorm(1, mu_sd2, sd_mu_sd2)
    sd_mu3[i] <- rnorm(1,  mu_sd3, sd_mu_sd3)

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


  delta1 <- rnorm( nBirds, mu_delta1, sd_delta1 )
  delta2 <- rnorm( nBirds, mu_delta2, sd_delta2 )

  birds = data.frame(mu1, mu2, mu3, sd_mu1, sd_mu2, sd_mu3, delta1, delta2)

  # for ( row in 1:nBirds ) {
  #   while ( birds[row, "mu1"] < 0 ){
  #     birds[row, "mu1"] <- rnorm(1, mu_mu1, sd_mu_mu1)
  #   }
  #   while ( birds[row, "mu2"] < 0 ){
  #     birds[row, "mu2"] <- rnorm(1, mu_mu2, sd_mu_mu2)
  #   }
  #   while ( birds[row, "mu3"] < 0 ){
  #     birds[row, "mu3"] <- rnorm(1, mu_mu3, sd_mu_mu3)
  #   }
  # }

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
simPopulationData <- function( birds, tStep, tSpan ){

  n <- nrow( birds )
  dat <- vector( mode = "list", length = n)

  for ( row in 1:n ){

      newBird <- simBirdData( tStep, tSpan, birds[row, "mu1"], birds[row, "mu2"], birds[row, "mu3"], birds[row, "sd_mu1"], birds[row, "sd_mu2"], birds[row, "sd_mu3"], birds[row, "delta1"] , birds[row, "delta2"])
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
library
  pm <- pars_mat

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
  mu_delta1 = pm[i, "mu_delta1"]
  mu_delta2 = pm[i, "mu_delta2"]
  sd_delta1 = pm[i, "sd_delta1"]
  sd_delta2 = pm[i, "sd_delta2"]
  tStep = pm[i, "tStep"]
  tSpan = pm[i, "tSpan"]

  mu_mu <- c(mu_mu1, mu_mu2, mu_mu3)
  mu_mu_delta <- c( mu_delta1, mu_delta2)
  sd_mu_delta <- c( sd_delta1, sd_delta2)
  sd_mu <- c(sd_mu1, sd_mu2, sd_mu3)

  n = tSpan / tStep

  birdPop <- simBirdParams( nBirds = nBirds, mu_mu1 = mu_mu1, mu_mu2 = mu_mu2, mu_mu3 = mu_mu3, sd_mu_mu1 = sd_mu1, sd_mu_mu2 = sd_mu2, sd_mu_mu3 = sd_mu3, mu_sd1 = mu_sd1, mu_sd2 = mu_sd2, mu_sd3 = mu_sd3, sd_mu_sd1 = sd_mu_sd1, sd_mu_sd2 = sd_mu_sd2, sd_mu_sd3 = sd_mu_sd3, mu_delta1 = mu_delta1, mu_delta2 = mu_delta2, sd_delta1 = sd_delta1, sd_delta2 = sd_delta2)

  birdDat <- simPopulationData( birdPop, tStep = tStep, tSpan = tSpan)

  birdDat <- simPopulationData( birdPop, tStep = tStep, tSpan = tSpan )

  saveRDS(birdDat, file = paste("Data", toString(i), ".RDS", sep = ""))

  load.module("glm")

  init_vals <- sim_init_vals(i)

  y <- array( dim = c( nBirds, n))
  t <- array( dim = c( nBirds, n))

  for ( j in 1:nBirds ){
    y[j, ] <- birdDat[[i]]$msrmnts
    t[j, ] <- birdDat[[i]]$times
  }

  ## USE PARAMS AS INIT DATA FOR NOW
  dat <- list( "y" = y, "t" = t, "n" = n, "nBirds" = nBirds, "mu_mu_delta" = mu_mu_delta, "sd_mu_delta" = sd_mu_delta, "mu_mu" = mu_mu, "sd_mu" = sd_mu)

  model <- jags.model("populationModel.txt", data = dat, n.chains = 3, n.adapt = 1000)
  monitor <- coda.samples(model, variable.names = c("mu_delta"), n.iter = 5000)

  summary <- summary(monitor)

  saveRDS(summary, file = paste("Summary", toString(i), sep = ""))
}

#' sim_init_vals generates initial values for the parallel processed chains
#'
#' @param i is the number of chains being run
#'
#' @return init_vals is a matrix of initial values, rows coressponding to sets of initial values
#' @export
#'
#' @examples
sim_init_vals <- function(i){

  init_vals <- vector( mode = "list", length = i)
  for (k in 1:i){

    mu_mu_delta <- vector(mode = "integer", length = 2)
    sd_mu_delta <- vector(mode = "integer", length = 2)
    mu_mu <- vector(mode = "integer", length = 3)
    sd_mu <- vector(mode = "integer", length = 3)

    mu_mu[1] <- -50
    mu_mu[2] <- -25
    mu_mu[3] <- -50

    sd_mu[1] <- 50
    sd_mu[2] <- 50
    sd_mu[3] <- 50

    mu_mu_delta[1] <- 6
    mu_mu_delta[2] <- 18

    sd_mu_delta[1] <- 6
    sd_mu_delta[2] <- 6

    init_vals[[k]] = list(mu_mu_delta, sd_mu_delta, mu_mu, sd_mu, sd_mu_delta)
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
#' @param mu_delta1 mean of first change point
#' @param mu_delta2 mean of second change point
#' @param sd_delta1 sd of first change point
#' @param sd_delta2 sd of second change point
#'
#' @return pm is a matrix of paramter values with each row a specific population
#' @export
#'
#' @examples
sim_pars_mat <- function( nPops, nBirds, tStep, tSpan, mu_mu1, mu_mu2, mu_mu3, sd_mu_mu1, sd_mu_mu2, sd_mu_mu3, mu_sd1, mu_sd2, mu_sd3, sd_mu_sd1, sd_mu_sd2, sd_mu_sd3, mu_delta1, mu_delta2, sd_delta1, sd_delta2 ){
  pm = matrix( nrow = nPops, ncol = 19, dimnames = list(c(1:6), c("nBirds" , "mu_mu1", "mu_mu2", "mu_mu3", "sd_mu_mu1", "sd_mu_mu2", "sd_mu_mu3", "mu_sd1", "mu_sd2", "mu_sd3", "sd_mu_sd1", "sd_mu_sd2", "sd_mu_sd3", "mu_delta1", "mu_delta2", "sd_delta1", "sd_delta2", "tStep", "tSpan")))
  for ( j in 1:nPops ){
    pm[j, "nBirds"] = nBirds
    pm[j, "mu_mu1"] = nudge(mu_mu1)
    pm[j, "mu_mu2"] = nudge(mu_mu2)
    pm[j, "mu_mu3"] = nudge(mu_mu3)
    pm[j, "sd_mu_mu1"] = nudge(sd_mu_mu1)
    pm[j, "sd_mu_mu2"] = nudge(sd_mu_mu2)
    pm[j, "sd_mu_mu3"] = nudge(sd_mu_mu2)
    pm[j, "mu_sd1"] = nudge(mu_sd1)
    pm[j, "mu_sd2"] = nudge(mu_sd2)
    pm[j, "mu_sd3"] = nudge(mu_sd3)
    pm[j, "sd_mu_sd1"] = nudge(sd_mu_sd1)
    pm[j, "sd_mu_sd2"] = nudge(sd_mu_sd2)
    pm[j, "sd_mu_sd3"] = nudge(sd_mu_sd3)
    pm[j, "mu_delta1"] = rbeta(1,2,2, ncp = 0) * 12
    pm[j, "mu_delta2"] = 12 + rbeta(1,2,2, ncp = 0 ) * 12
    pm[j, "sd_delta1"] = nudge(sd_delta1)
    pm[j, "sd_delta2"] = nudge(sd_delta2)
    pm[j, "tStep"] = tStep
    pm[j, "tSpan"] = tSpan
  }
  return(pm)
}

#' nudge is used to perturb a given value by up to +/- 50%
#' @param val the value to perturb
#'
#' @return val the perturbed value
#' @export
#'
#' @examples
nudge <- function(val){
  val <- val + val * ( rbeta(1,2,2, ncp = 0 ) - 0.5 )
  return( val )
}


