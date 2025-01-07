changeLevelNamesDT <- function(inputDT, categoricalVar, oldLevelName, newLevelName, dropCheck = FALSE){
  
  checkCharVec(list(categoricalVar, oldLevelName, newLevelName))
  checkDT(inputDT, categoricalVar)
  checkLength(list(categoricalVar), 1)
  checkEqualLength(list(oldLevelName, newLevelName))
  
  indexFactor <- which(names(inputDT) %in% categoricalVar)
  noFactor <- !isFactorDT(inputDT[,.SD,.SDcols = indexFactor], NULL, FALSE)
  if(sum(noFactor) != 0) stop(paste(paste('The variable ', categoricalVar[noFactor], sep = ''), ' of the "inputDT" argument is not a factor.', sep = ''))
  
  selectedFactor <- inputDT[,.SD,.SDcols = indexFactor]
  if(!dropCheck){
    for(iRun in 1:length(oldLevelName)){
      if(!isPresentCharVec(levels(selectedFactor[[1]]), oldLevelName[iRun])) stop(sprintf("The value %s of the %dth element of the 'oldLevelName' argument does not correspond to a valid level of the 'categoricalVar' argument, %s.", oldLevelName[iRun], iRun, categoricalVar))
    }
  }
  levels(selectedFactor[[1]]) <- c(levels(selectedFactor[[1]]), newLevelName)
  for(iRun in 1:length(oldLevelName)){
    set(selectedFactor, as.integer(which(selectedFactor[[1]] == oldLevelName[iRun])), 1L, newLevelName[iRun])
  }
  set(inputDT, NULL, as.integer(indexFactor), selectedFactor)
  removeEmptyLevelsDT(inputDT, categoricalVar)
  
}

