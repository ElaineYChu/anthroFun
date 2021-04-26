#' @title Collapsing left and right columns in data and output as a new data frame
#' 
#' @description User-controlled process to collapse bilateral data into
#' "collapsed" traits for use in statistical analyses. Collapsing choices include:
#' - Continuous data: 'lefts', 'rights', 'mean'   
#' - Ordinal data: 'highest', 'lowest' expression, 'none' (unilateral trait)  
#' 
#' The function outputs a new data frame with a single column per traitVec, collapsing
#' the sides based on user-defined "choose" option for each trait.
#' 
#' @param inputDf Original data frame to be manipulated.
#' @param keepCols Column numbers to keep in final output without manipulation (ex. 1:4).
#' @param sideLabels Character string of left and right (in that order) labels (ex. c('L','R')).
#' @param sideLoc Character string identifying if side labels are at the "start" or "end" of trait names.
#' @param traitVec Vector of trait names that need collapsing without left or right distinction (ex. c('FDL','HDL')).
#' @param approach Vector of choices for each trait (ex: 'lefts', 'highest', 'none'). This Vector should be the same length as 'traitVec'.
#' 
#' @examples 
#' US <- read.csv('https://github.com/ElaineYChu/stull_mcp/tree/main/inst/extdata/US.csv')  # import data from stull_mcp github repository
#' sub <- US[c(1:4, 8, 13, 84, 85)]  # subset columns for demonstration
#' head(sub)  # inspect subset
#' ex <- collapseData(inputDf=sub, keepCols=1:4, sideLabels=c('_L','_R'), sideLoc='end', traitVec=c('FDL','man_I1'), approach=c('lefts','highest'))  # keep the first 4 columns, collapse 'FD" and 'man_I1' using the corresponding approaches
#' head(ex)
#' 
#' @export

collapseData <- function(inputDf, keepCols, sideLabels, sideLoc, traitVec, approach) {
  
  if(length(traitVec)!=length(approach)){
    stop("'traitVec' and 'approach' are not of the same length")
  }
  if(length(sideLabels)!=2) {
    stop("One or more side labels are missing")
  }
  if(length(grep('l', sideLabels[1], ignore.case=TRUE)) < 1) {
    stop("Left label not provided first in 'sideLabels'.")
  }
  if(sideLoc!= 'start' & sideLoc!= 'end') {
    stop("Unsupported side location ('sideLoc')")
  }
  
  keepDf <- inputDf[keepCols]  # save the user-defined columns
  leftLab <- sideLabels[1]
  rightLab <- sideLabels[2]
  
  traitCols <- NULL  # initialize empty trait column vector
  
  for(i in 1:length(traitVec)) {
    traitCols <- c(traitCols,grep(traitVec[i],names(inputDf)))  # trait column numbers
  }
  
  inputSub <- inputDf[traitCols]  # subset selected traits
  
  for(j in 1:length(traitVec)) {
    curTrait <- traitVec[j]
    curChoice <- approach[j]
    
    if(curChoice == 'none') {
      keepDf[curTrait] <- inputSub[[curTrait]]  # unilateral trait, append as new column to keep df
    } else {
      if(sideLoc == 'start') {
        curTraitL <- paste0(leftLab,curTrait)
        curTraitR <- paste0(rightLab,curTrait)
      } else{
        curTraitL <- paste0(curTrait,leftLab)
        curTraitR <- paste0(curTrait, rightLab)
      }
      leftSide <- inputSub[[curTraitL]]
      rightSide <- inputSub[[curTraitR]]
      
      if(curChoice == 'lefts') {
        keepDf[curTrait] <- ifelse(is.na(leftSide), rightSide, leftSide)
      } else if(curChoice == 'rights') {
        keepDf[curTrait] <- ifelse(is.na(rightSide), leftSide, rightSide)
      } else if(curChoice == 'mean') {
        keepDf[curTrait] <- rowMeans(cbind(leftSide, rightSide), na.rm=TRUE)
      } else if(curChoice == 'highest') {
        keepDf[curTrait] <- pmax(leftSide, rightSide, na.rm=TRUE)
      } else if(curChoice == 'lowest') {
        keepDf[curTrait] <- pmin(leftSide, rightSide, na.rm=TRUE)
      }
    }
  }
  
  return(keepDf)
}

## EXAMPLE
# Simulate data for sample data frame
ogDf <- data.frame(ID=letters[1:10],
                   sex=rep(c('M','F'),length.out=10),
                   age=runif(10,min=0,max=15),
                   lb_L=runif(10,min=150,max=350),
                   lb_R=runif(10,min=150,max=350),
                   Lef=floor(runif(10,min=0,max=4)),
                   Ref=floor(runif(10,min=0,max=4)),
                   uniTrait=floor(runif(10,min=0,max=8)),
                   dent_L=ceiling(runif(10,min=1,max=13)),
                   dent_R=ceiling(runif(10,min=1,max=13)))
head(ogDf)  # view a subset of the data

# Keep demographic columns (ID, sex, age), Collapse L/R for lb and dent, and keep uniTrait as is
newDf1 <- collapseData(inputDf=ogDf, keepCols=1:3, sideLabels=c('_L','_R'), sideLoc='end', traitVec=c('lb','uniTrait','dent'), approach=c('lefts','none','highest'))
head(newDf1)

# Keep demographic columns (ID, sex), Collapse L/R for ef
newDf2 <- collapseData(inputDf=ogDf, keepCols=1:2, sideLabels=c('L','R'), sideLoc='start', traitVec=c('ef'), approach=c('lowest'))
head(newDf2)
