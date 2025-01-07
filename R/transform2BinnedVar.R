#' Categorization of continuous according to some splitting points.
#'
#' @param inputDT data.table object containing the data of interest. This is an obligatory argument, without default value.
#' @param splits List of numeric vectors, ideally being the output object of the 'groupingVars' function. The names of this object need to correspond to column names of the 'inputDT' argument. This is an obligatory argument, without default value.
#' @param removeAlreadyPresent Logical vector of length 1 indicating whether or not continuous variables of the 'inputDT' argument that were already categorized by this function during a previous call, and which are about to be recategorized again, should remove or not the categorization of the previous call. The default value is TRUE, and is an optional argument.
#' @return The variables whose names are found in the 'split' argument as names of the object that is passed to this argument, are splitted by means of the splitting points that are supplied by the same 'split' argument. The resulting variable is added to the object that is passed to the 'inputDT' argument, and the respective column name corresponds to the variable name augmented with the suffix 'Grouped', without any space or character in between. Note that the grouping is always performed in the following manner: if the 'split' argument supplies the splitting points a, b, and c, then the continuous variable is split up in the following intervals ]-infinity, a[, [a, b[ and [b, +infinity[.
#' @examples

transform2BinnedVar <- function(inputDT, 
                                splits, 
                                removeAlreadyPresent = TRUE){
  
  checkLogicVec(list(removeAlreadyPresent))
  if(!is.list(splits)) stop("The 'split' argument is a list of numeric vectors.")
  
  varNames <- names(splits)
  checkDT(inputDT, varNames)
  checkLogicVec(list(removeAlreadyPresent))
  checkLength(list(removeAlreadyPresent), 1)
  
  colsAlreadyTransformed <- which(names(inputDT) %in% paste(varNames, 'Grouped', sep = ''))
  if(length(colsAlreadyTransformed) > 0) set(inputDT, NULL, colsAlreadyTransformed, NULL)
  nColOriginal <- ncol(inputDT)
  varNamesOriginal <- names(inputDT)
  for(iVar in 1:length(varNames)){
    
    if(removeAlreadyPresent){
      alreadyPresentCol <- which(names(inputDT) %in% c(paste(varNames[iVar], 'Grouped', sep = ''), 'newcol'))
      if(length(alreadyPresentCol) != 0){
        set(inputDT, NULL, alreadyPresentCol, NULL)
        nColOriginal <- nColOriginal - 1
      }
    }
    selectedVar <- inputDT[, .SD, .SDcol = which(varNamesOriginal == varNames[iVar])]
    selectedVarGrouped <- selectedVar
    if(sum(names(splits[[iVar]]) %in% c('splits')) > 0){
      selectedSplits <- sort(splits[[iVar]]$splits)
    } else {
      selectedSplits <- sort(splits[[iVar]])
    }
    if(sum(is.na(selectedSplits)) == 0){
      nGroups <- length(selectedSplits) + 1
      for(iSplit in (nGroups - 1):1){
        selectedVarGrouped[selectedVar < selectedSplits[iSplit]] <- iSplit
      }
      selectedVarGrouped[selectedVar >= selectedSplits[nGroups-1]] <- nGroups
      
      inputDT[ , newcol := character(.N)]
      set(inputDT, NULL, as.integer(nColOriginal + iVar), as.factor(selectedVarGrouped[[1]]))
      setnames(inputDT, 'newcol', paste(varNames[iVar], 'Grouped', sep = ''))
    } else {
      warning(paste("No splits were provided by the 'splits' argument for the variable ", varNames[iVar], sep = ''))
    }
  }
}