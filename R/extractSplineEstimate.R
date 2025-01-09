#' Extract the Spline estimates of the GAM model 
#'
#' @param gamFit object gam, with the already runned GAM model of [mgcv::gam()]
#' @param keyWordColSelection list with the smooth terms to be extracted from the GAM. 
#' If NULL all the smooth terms will be used
#'
#' @returns list of length of the `keyWordColSelection` containing a list with all the estimates in the GAM. 
#' The number of lines will then be the number of rows of the given dataset. 
#' @export
extractSplineEstimate <- function(gamFit, 
                                  keyWordColSelection = NULL){
  
  allSmoothTerms <- laply(gamFit$smooth, function(xx) xx$vn[1])

  if(is.null(keyWordColSelection)){
    keyWordColSelection <- allSmoothTerms
  } else {
    present <- (keyWordColSelection %in% allSmoothTerms)
    if(!all(present)){
      sprintf("The elements %s of the 'keyWordColSelection' argument are not part of the smooth terms of the 'gamFit' argument.", 
              paste(keyWordColSelection[!present], collapse = ', '))
    }
  }
  
  if (!any(class(gamFit) %in% 'gam')) {
    stop('The "gamFit" argument should be an output object of the mgcv::gam function.')
  } 
  gamFitModelMatrix <- model.matrix(gamFit)
  
  fittedSpline <- list()
  length(fittedSpline) <- length(keyWordColSelection)
  
  for (iSpline in 1:length(fittedSpline)) {
    selectionCols <- grep(keyWordColSelection[iSpline], names(gamFit$coefficients))
    fittedSpline[[iSpline]] <- gamFitModelMatrix[, selectionCols] %*% 
      gamFit$coefficients[selectionCols]
  }
  names(fittedSpline) <- keyWordColSelection
  return(fittedSpline)
}

