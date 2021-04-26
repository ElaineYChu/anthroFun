##############################################################
##
##      Collapse the Lefts and Rights of a trait
##     together into a single column, based on the
##              user's specifications. 
##
##  Included in package on Github: MichaelHoltonPrice/yada
##
##############################################################

## Argument Descriptions:
#' @param inputDf Data frame with original data, including traits with Left and Rights
#' @param keepCols Vector of column numbers that should be kept, but not manipulated. Ex: keepCols=1:3
#' @param sideLabels Vector of how the Left and Right sides are denoted. Ex: sideLabels=c('L','R')
#' @param sideLoc Location of the side labels. The two options are: sideLoc='start' or sideLoc='end'
#' @param traitVec Vector of final traits to be collapsed or kept, as defined by the user. 
#' Ex: traitVec=c('FDL','RDL','man_M1','max_M2','carpal_count')
#' @param approach Vector of how each trait should be collapsed. This vector length should be
#' equal to the length of traitVec. Current options are: approach=
#' 'mean' (recommended only for continuous traits)
#' 'lefts' (prefer left, substitute with right)
#' 'rights' (prefer right, subsitute with left)
#' 'highest' (highest value or trait expression)
#' 'lowest' (lowest value or trait expression)


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
