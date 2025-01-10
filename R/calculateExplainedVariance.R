#' Calculate the Explained Variance 
#'
#' @param inputDT data.table with the input data
#' @param splineEst named list of the spline estimates of the GAM model 
#' @param varName column name of `splits` to get the estimates from 
#' @param groupedVarName column based on `varName` which is already grouped
#' 
#' @inheritParams transform2BinnedVar
#'
#' @returns single numeric value, which is the total explained variance 
#' @export
calculateExplainedVariance <- function(inputDT, 
                                       splits, 
                                       splineEst, 
                                       varName, 
                                       groupedVarName = sprintf("%sGrouped", varName)){
    
    transform2BinnedVar(inputDT, splits)
    levs <- extractLevelDT(inputDT, groupedVarName)[[1]]
    sdGroup <- rep(NA, length(levs))
    nObs <- rep(NA, length(levs))
    
    for (iLev in 1:length(levs)) {
      workDT <- copy(inputDT)
      
      workDT <- workDT[as.integer(rownames(splineEst[[varName]])), ]
      
      if (varName == 'zone') {
        workDT[, splineEst := splineEst[which(names(splineEst) %in% c('longitude', 'latitude'))][[1]]]
      } else {
        workDT[, splineEst := splineEst[varName]]
      }
      
      # select only the results of the specified variable
      splineEstSubset <- subsetDT(workDT, 
                                  which(names(workDT) == groupedVarName), 
                                  levs[iLev])$splineEst
      
      sdGroup[iLev] <- sd(splineEstSubset, na.rm = T)
      nObs[iLev] <- length(splineEstSubset)
    }
    
    if (varName == 'zone') {
      explVar <- 1 - (sum(sdGroup*nObs)/sum(nObs))/sd(splineEst[which(names(splineEst) %in% c('longitude', 'latitude'))][[1]])
    } else {
      explVar <- 1 - (sum(sdGroup*nObs)/sum(nObs))/sd(splineEst[[varName]])
    }
  
    return(explVar)
}
