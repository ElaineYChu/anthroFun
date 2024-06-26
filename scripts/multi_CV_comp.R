##############################################################
##
##      Conduct multiple, user-defined comparisons for
##     coefficient of variation between age groups and/or
##        other defined groups. This functions uses the
##      `mslrt_test()` function in package `cvequality`
##              by Krishnamoorthy and Lee (2014)
##
##     This function is not included in any package, and was
##        designed for use in the dissertation analyses
##                      of Elaine Y. Chu
##
##############################################################

## Argument Descriptions:
#' @param dataframe Input data frame with groups and continuous variabes as columns
#' and individuals as rows.
#' @param varVec Vector of the names of continuous variables to be compared.
#' Ex: varVec=c('FDL','TDL','HDL','RDL')
#' @param ageVec Vector of the ages or age cohorts to be compared.
#' Ex1: ageVec=c('Infant','Child','Juvenile','Adolescent')
#' Ex2: ageVec=c(1,2,3,4,5,6,7)
#' @param grpVec Vector of other groupings (not age) to be compared. Default is popVec=NA,
#' which conducts no between-group comparisons.
#' Ex: grpVec=c('US','SA','FR','TW')
#' @param btwGrp Logical for whether among-group comparisons should be conducted.
#' Default is btwGrp=F, where only within-group comparisons are made.
#' @param alpha Threshold for significance. Default is alpha=0.05.
#' @param seed Random value to allow for repeatability of mslrt comparisons.
#' Default is seed=2021, but can be any number as long as it is consistent between runs.


multi_CV_comp <- function(dataframe, varVec, ageVec, grpVec=NA, btwGrp=F, alpha=0.05, seed=2021){
  set.seed(seed)
  
  ## Initialize empty vectors for storage
  var <- NULL
  comp <- NULL
  popCol <- NULL  # may or may not be used
  mslrt <- NULL
  p <- NULL
  sig <- NULL
  
  ## Loop through long bones (varVec), age comparisons (ageVec), and pop comparisons (grpVec, btwGrp)
  for(i in 1:length(varVec)){
    if(isFALSE(btwGrp)){
      for(j in 1:(length(ageVec)-1)){
        ages <- as.character(c(ageVec[j], ageVec[j+1]))
        var <- c(var,varVec[[i]])
        
        ## If-else
        if(length(grpVec)==1){  # sample-wide analysis
          comp <- c(comp, paste(ages[1],ages[2],sep='-'))
          temp <- dataframe %>% select(agey, varVec[i]) %>% filter(agey %in% ages) %>% drop_na()  # filter for ageVec comparisons by lb
          
          res <- tryCatch({
            mslr_test(nr=1e4, x=temp[[2]], y=temp[[1]])  # x=lb, y=agey
          },
          error=function(cond){
            return(NA)  # if error occurs when attempting mslr_test, then return NA for res
          }
          )
          if(is.na(res[[1]])){
            mslrt <- c(mslrt, NA)
            p <- c(p, NA)
            sig <- c(sig, NA)
          } else{
            mslrt <- c(mslrt, round(res[[1]],2))
            p <- c(p, signif(res[[2]],3))
            sig <- c(sig, ifelse(res[[2]]<alpha, T, F))
          }
        } else{
          for(k in 1:length(grpVec)){
            temp <- dataframe %>% select(agey, pop, varVec[i]) %>% filter (agey %in% ages, pop == grpVec[[k]]) %>% drop_na()  # filter for pop-specific ageVec comparisons by lb
            comp <- c(comp, paste0(ages[1], '-', ages[2]))
            popCol <- c(popCol, grpVec[[k]])
            
            res <- tryCatch({
              mslr_test(nr=1e4, x=temp[[3]], y=temp[[1]])  # x=lb, y=agey
            },
            error=function(cond){
              return(NA)  # if error occurs when attempting mslr_test, then return NA for res
            })
            if(is.na(res[[1]])){
              mslrt <- c(mslrt, NA)
              p <- c(p, NA)
              sig <- c(sig, NA)
            } else{
              mslrt <- c(mslrt, round(res[[1]],2))
              p <- c(p, signif(res[[2]],3))
              sig <- c(sig, ifelse(res[[2]]<alpha, T, F))
            }
          }
        }
      }  # close ages forloop
    } else{  # single-age, between population comparisons
      for(m in 1:length(ageVec)){
        temp <- dataframe %>% select(agey, pop, varVec[i]) %>% filter(agey == ageVec[m]) %>% drop_na()  # filter for specific age
        comp <- c(comp, ageVec[m])
        var <- c(var,varVec[[i]])
        
        res <- tryCatch({
          mslr_test(nr=1e4, x=temp[[3]], y=temp[[2]])  # x=lb, y=pop
        },
        error=function(cond){
          return(NA)  # if error occurs when attempting mslr_test, then return NA for res
        })
        if(is.na(res[[1]])){
          mslrt <- c(mslrt, NA)
          p <- c(p, NA)
          sig <- c(sig, NA)
        } else{
          mslrt <- c(mslrt, round(res[[1]],2))
          p <- c(p, signif(res[[2]],3))
          sig <- c(sig, ifelse(res[[2]]<alpha, T, F))
        }
      }  # close ageVec forloop
    }  # close else statement
  }  # close varVec forloop
  
  ## Create final df and print significant relationships
  if(length(popCol)>1){
    df <- data.frame(var, comp, popCol, mslrt, p, sig)
  } else{
    df <- data.frame(var, comp, mslrt, p, sig)
  }
  
  #ifelse(isFALSE(btwGrp) & length(grpVec)>1, df <- data.frame(var, comp, popCol, mslrt, p, sig), df <- data.frame(var, comp, mslrt, p, sig))
  
  print('Calculations Complete! The following relationships are significant:')
  df %>% filter(sig==TRUE) %>% print()
  
  return(df)
}