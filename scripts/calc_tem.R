###########################################
##
##     Function for calculating
##    Technical Error of Measurement 
##             (TEM, %TEM)
##
##        By: Elaine Y. Chu
##     Completed on: July 10, 2021
##
###########################################

#' @title Technical Error of Measurement (TEM)
#' 
#' @description Generic function to calculate the technical error of
#' measurement (TEM) and relative technical error of measurement (%TEM)
#' commonly used to evaluate intra- and inter-observer error in
#' anthropology and anthropometry.
#' 
#' @param user_1 A vector of length (n) with the first set of observations.
#' @param user_2 A vector of length (n) with the second set of observations.
#' @param mode Selection for either the "absolute" (TEM) or "relative" (%TEM)
#' calculation. Default is "absolute".
#' @param decimals Number of decimal places for the final calculation to round.
#' Default is 2.
#' 
#' @returns A value for either TEM or %TEM. 
#' 
#' @examples 
#' Sally <- c(1:15)
#' Max <- c(1:15)*1.02
#' 
#' calc_tem(Sally, Max, "absolute", decimals=2)  # TEM = 0.13
#' calc_tem(Sally, Max, "relative", decimals=4)  # %TEM = 1.5914
#' 
#' ## This Example Should Fail with "Unequal number of observations":
#' calc_tem(Sally[1:6], Max)
#' 
#' ## This Example Should Fail with "Not a valid mode":
#' calc_tem(Sally, Max, "TEM")


calc_tem <- function(user_1, user_2, mode="absolute", decimals=2) {
  if(length(user_1) != length(user_2)) {
    stop("Unequal number of observations")
  }
  
  if(mode != "absolute" & mode != "relative") {
    stop("Not a valid mode")
  }
  
  ## Calculate absolute tem
  numerator <- sum((user_1 - user_2)^2)
  n <- length(user_1)
  denominator <- 2 * n
  atem <- sqrt(numerator / denominator)
  
  if(mode == "absolute") {
    return(paste0("TEM = ", round(atem, decimals)))
  }
  
  if(mode == "relative") {
    vav <- mean(c(user_1, user_2))
    rtem <- (atem / vav) * 100
    return(paste0("%TEM = ", round(rtem, decimals)))
  }
}















