##############################################################
##
##      Remove any rows where all interested columns are NA.
##
##     This function is not included in any package, but was
##        designed for use in a number of grant analyses
##                        by Elaine Y. Chu
##
##############################################################

## Argument Description
#' @param df Input data frame to be edited
#' @param searchCols Vector of column names or numbers to look for all NAs
#' Ex1: searchCols=c('man_M1','max_I2','FDL','RDL')
#' Ex2: searchCols=5:8


removeNA <- function(df, searchCols) {
  allNA <- which(rowSums(is.na(df[searchCols]))==length(searchCols))  # row numbers where all are NA
  
  newDf <- df[-allNA,]  # remove rows where all are NA
  
  return(newDf)
}
