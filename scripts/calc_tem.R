###########################################
##
##     Function for calculating
##    Technical Error of Measurement 
##             (TEM, %TEM)
##
##        By: Elaine Y. Chu
##     Completed on: July 10, 2021
##  Updated on: April 19, 2024 - added 3+ raters capabilities
##
###########################################

#' @title Technical Error of Measurement (TEM)
#' 
#' @description Generic function to calculate the technical error of
#' measurement (TEM) and relative technical error of measurement (%TEM)
#' commonly used to evaluate intra- and inter-observer error in
#' anthropology and anthropometry.
#' 
#' @param data Dataframe containing each observer's measurements as columns.
#' @param mode Selection for either the "absolute" (TEM) or "relative" (%TEM)
#' calculation. Default is "absolute".
#' @param decimals Number of decimal places for the final calculation to round.
#' Default is 2.
#' 
#' @returns A list with either TEM or %TEM
#' 
#' @examples 
#' Sally <- c(1:15)
#' Max <- c(1:15)*1.02
#' Jack <- c(1:15)*0.98
#' 
#' inter <- data.frame(Sally, Max, Jack)
#' 
#' calc_tem(inter[1:2], "absolute", decimals=2)  # TEM = 0.13
#' calc_tem(inter[1:2], "relative", decimals=4)  # %TEM = 1.5914
#' 
#' calc_tem(inter)  # TEM = 0.18
#' calc_tem(inter, mode="relative", 4)  # %TEM = 2.273
#' 
#' ## This Example Should Fail with "Cannot calculate with missing data":
#' broken <- inter
#' broken[2,1] <- NA
#' calc_tem(broken)
#' 
#' ## This Example Should Fail with "Not a valid mode":
#' calc_tem(inter, "TEM")


calc_tem <- function(data, mode="absolute", decimals=2) {
  if(any(is.na(data))) {
    stop("Cannot calculate with missing data")
  }
  
  if(mode != "absolute" & mode != "relative") {
    stop("Not a valid mode")
  }
  
  ## Convert any weird data structures (aka tibbles) to data.frame
  data <- as.data.frame(data)
  
  ## Decide whether there are 2 or 3+ raters
  two_raters <- ifelse(ncol(data)==2, T, F)
  
  ## Calculate absolute tem
  ## If there are two raters only, calculate TEM normally
  ## Else, use the TEM extension``
  if(two_raters) {
    numerator <- sum((data[,1] - data[,2])^2)
    n <- nrow(data)
    denominator <- 2 * n
    atem <- sqrt(numerator / denominator)
  } else {
    N = nrow(data)
    K = ncol(data)
    
    atem <- sqrt(sum(apply(data,1,function(x) sum(x^2) - sum(x)^2/K))/(N*(K-1)))
  }
  
  if(mode == "absolute") {
    return(list(atem=round(atem, decimals)))
  }
  
  if(mode == "relative") {
    data_vec <- NULL
    for(i in 1:ncol(data)) {
      data_vec <- c(data_vec, data[ ,i])
    }

    vav <- mean(data_vec)
    rtem <- (atem / vav) * 100
    return(list(rtem=round(rtem, decimals)))
  }
}

