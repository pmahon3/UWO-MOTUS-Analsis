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

  times = seq( from = 1, to = tSpan, by = tStep)
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

#' simPopulation simulates the data for a population of n birds
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


simPopulationParams <- function( nBirds, mu_mu1, mu_mu2, mu_mu3, sd_mu_mu1, sd_mu_mu2, sd_mu_mu3, mu_sd1, mu_sd2, mu_sd3, sd_mu_sd1, sd_mu_sd2, sd_mu_sd3, mu_delta1, mu_delta2, sd_delta1, sd_delta2 ){

    #List of params for nBirds
    mu1 <- rnorm( nBirds, mu_mu1, sd_mu_mu1)
    mu2 <- rnorm( nBirds, mu_mu2, sd_mu_mu2)
    mu3 <- rnorm( nBirds, mu_mu3, sd_mu_mu3)

    sd_mu1 <- rnorm( nBirds, mu_sd1, sd_mu_sd1)
    sd_mu2 <- rnorm( nBirds, mu_sd2, sd_mu_sd2)
    sd_mu3 <- rnorm( nBirds, mu_sd3, sd_mu_sd3)

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

