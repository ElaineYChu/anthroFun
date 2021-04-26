# Main Objective
This repository houses a number of single-function scripts that were borne out of necessity by myself or my colleagues while conducting Anthropological Research. 

Each `.R ` script starts with a description that provides information about how each argument is used and should be formatted.

To use any of these functions, copy-and-paste the function script, run it, and you should be good to go.

If you run into any errors or issues that you cannot figure out, please feel free to email me at: [elainechu@nevada.unr.edu]
Enjoy!

# Example
The following is an example of what the 'function script' that needs to be copied, pasted, and run should look like:

``` r
## Function code for collapseScores
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
```
