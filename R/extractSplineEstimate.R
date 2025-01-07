extractSplineEstimate <- function(gamFit, keyWordColSelection = NULL){
  
  allSmoothTerms <- laply(gamFit$smooth, function(xx) xx$vn[1])
  allSmoothTerms[which(!allSmoothTerms %in% c('longitude', 'latitude'))]
  if(is.null(keyWordColSelection)){
    keyWordColSelection <- allSmoothTerms
  } else {
    present <- (keyWordColSelection %in% allSmoothTerms)
    if(length(present) != length(keyWordColSelection)){
      sprintf("The elements %s of the 'keyWordColSelection' argument are not part of the smooth terms of the 'gamFit' argument.", paste(keyWordColSelection[!present], collapse = ', '))
    }
  }
  if(sum(class(gamFit) %in% 'gam') == 0) stop('The "gamFit" argument should be an output object of the mgcv::gam function.')
  gamFitModelMatrix <- model.matrix(gamFit)
  fittedSpline <- list()
  length(fittedSpline) <- length(keyWordColSelection)
  for(iSpline in 1:length(fittedSpline)){
    selectionCols <- grep(keyWordColSelection[iSpline], names(gamFit$coefficients))
    fittedSpline[[iSpline]] <- gamFitModelMatrix[, selectionCols]%*%gamFit$coefficients[selectionCols]
  }
  names(fittedSpline) <- keyWordColSelection
  return(fittedSpline)
}

