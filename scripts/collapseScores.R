##############################################################
##
##                Collapse trait scores 
##     together into a single column, based on the
##              user's specifications. 
##
##  Included in package on Github: MichaelHoltonPrice/yada
##
##############################################################

## Argument Descriptions:
#' @param inputDf Data frame continaing traits as columns
#' @param trait Name of trait to be manipulated. Ex: trait='man_M2'
#' @param scores String of possible trait scores in order, with curly 
#' brackets denoting the scores that should be collapsed together.
#' Ex1: scores='1,{2,3,4},5,6,7,{8,9,10,11},{12,13}'
#' Ex2: scores='1,{12,2,23},3,4'

## NOTE: This function cannot handle a scores argument with gaps.
## Ex: '-1,1,2,3,{4,5,6}'

collapseScores <- function(inputDf, trait, scores) {
  cList <- stringr::str_extract_all(scores, "(?<=\\{).+?(?=\\})")[[1]]  # extract collapsing groups
  cGroups <- strsplit(cList, ',')  # split collapsed groups by commas
  fullList <- stringr::str_remove(strsplit(scores,",")[[1]],"[{]|[}]")  # ordered trait list
  expScoreN <- length(fullList) - (length(unlist(cGroups))-length(cGroups))  # expected final number of score options
  minScore <- min(as.numeric(fullList))
  finalScores <- minScore:(ifelse(minScore==0, (expScoreN-1), (expScoreN)))  # final score list
  
  traitVec <- inputDf[[trait]]  # original trait vector
  
  for(g in 1:length(cGroups)) {
    cGroups[[g]] <- as.numeric(cGroups[[g]])  # convert values to numeric
  }
  
  cTraits <- traitVec  # initialize vector for collapsing
  
  for(i in 1:length(cGroups)) {
    cScores <- cGroups[[i]]
    cTraits <- ifelse(cTraits %in% cScores, min(cScores), cTraits)  # rescore defined scores only
  }
  
  
  for(f in 1:length(cGroups)) {
    traitScores <- sort(unique(na.omit(cTraits)))  # unique scores for trait
    if(all.equal(traitScores,finalScores) != TRUE) {  # remap needed
      idxOff <- which(traitScores != finalScores)
      
      diff <- traitScores[idxOff][1] - finalScores[idxOff][1]
      
      cTraits[cTraits %in% traitScores[idxOff]] <- cTraits[cTraits %in% traitScores[idxOff]] - diff
    } else {
      break
    }
  }
  return(cTraits)
}


## EXAMPLE 1 - Single trait, no recoding needed
# Simulate data for sample data frame
ogDf <- data.frame(max_M1=1:13,
                   max_M2=1:13,
                   man_C=1:13,
                   EF=rep(c(1:4,12,23),length.out=13))
ogDf  # view data

# Recode EF so that 12 is now a 2, and 23 is now a 3
newEF <- collapseScores(inputDf=ogDf, trait='EF', scores='1,{12,2},{23,3},4')
cbind(ogDf$EF, newEF)  # compare old scores to new scores

## EXAMPLE 2 - Multiple traits with same recoding / remapping needed
# Collapse dental development stages 2, 3, and 4 together and 10, 11, 12 together, and recode other traits
traitNames <- c('max_M1','max_M2','man_C')  # dental trait names
newDf <- data.frame(max_M1=rep(NA,13),  # empty data frame
                    max_M2=rep(NA,13),
                    man_C=rep(NA,13))

# Loop through traits to perform recoding process
for(i in 1:length(traitNames)) {
  newDf[traitNames[i]] <- collapseScores(inputDf=ogDf, trait=traitNames[i], scores='1,{2,3,4},5,6,7,8,9,{10,11,12},13')
}

newDf  # view newDf
cbind(ogDf$max_M1,newDf$max_M1)  # compare old max_M1 to new max_m1
cbind(ogDf$man_c,newDf$man_C)  # compare old man_C to new man_C
