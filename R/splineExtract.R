#' Extract the spline for a variable of a GAM 
#'
#' @param gamFit gam object
#' @param selectedVar name of the variable from the data to create the GAM, which is a spline 
#' @param min_max_values `numeric(2)` with the minimum and maximum values of `selectedVar`. 
#' @param nSamples positive integer, 
#' the number of splits
#' @param simultaneousCI logical, 
#' if the confidence interval should be simulated
#'
#' @returns data.table with columns `xVar`, `yVar`, `upperbound` and `lowerbound`
#' `xVar` are the values of the selected variable after transformation for the GAM 
#' `yVar` the spline results 
#' `upperbound` and `lowerbound` are the boundaries of the Confidence Interval
#' @export
splineExtract <- function(gamFit, 
                          selectedVar, 
                          min_max_values, 
                          nSamples = 1000, 
                          simultaneousCI = FALSE){
  
  # start checks
  
  if (sum(class(gamFit) %in% 'gam') == 0) {
    stop('The "gamFit" argument should be an output object of the mgcv::gam function.')
  } 
  if (length(selectedVar) > 1){
    stop('The "selectedVar" argument should only 1 variable to plot')
  }
  
  if (!(selectedVar %in% attr(gamFit$terms, 'term.labels'))) {
    stop(sprintf("%s is not a model variable of 'gamFit' with smoothed results", 
                 selectedVar))
  }
  
  # end checks
  
  includedVars <- attr(gamFit$terms, 'dataClasses')
  # extract the numerical values - without the offset
  numericVarNames <- names(includedVars)[which(includedVars == 'numeric')]
  numericVarNames <- numericVarNames[!grepl('offset', numericVarNames)]
  # extract the categorical values - factors or characters
  categoricalVarNames <- names(gamFit$xlevels)
  
  # get the reference classes for the categories, to be set as base for the new data
  referenceClass <- laply(gamFit$xlevels, function(xx) xx[1])
  
  # struct base data frame to be filled in
  refDF <- data.frame(t(c(1, rep(0, length(numericVarNames)), referenceClass)))
  names(refDF) <- c('exposure', numericVarNames, categoricalVarNames)
  
  coVarMatrix <- vcov(gamFit)
  
  # create the sequence for the points to get the spline of
  newDataVar <- seq(min(min_max_values), 
                    max(min_max_values), 
                    length = nSamples)
  
  newData <- data.frame(refDF, 
                        temp = rep(-1, length(newDataVar)))
  newData <- newData[,-ncol(newData)]
  newData[, which(names(refDF) == selectedVar)] <- newDataVar
  newData <- as.data.table(newData)
  
  categoricalCols <- which(names(newData) %in% names(gamFit$xlevels))
  
  numericCols <- 1:ncol(newData)
  numericCols <- numericCols[-categoricalCols]
  names(newData)[numericCols]
  
  for(iCol in numericCols){
    newData[[iCol]] <- as.numeric(newData[[iCol]])
  }
  
  predictions <- predict(gamFit, 
                         newData, 
                         type = "terms", 
                         se.fit = TRUE)
  fitSE <- predictions$se.fit
  
  selectedVarSpline <- sprintf("s(%s)", selectedVar) 
  selectedCol <- grep(selectedVarSpline, 
                      colnames(fitSE), 
                      fixed = TRUE)
  stError <- fitSE[, selectedCol]
  
  
  Xp <- predict(gamFit, 
                newdata = newData, 
                type = "lpmatrix")
  
  selectedCol <- grep(selectedVarSpline, 
                      colnames(Xp), 
                      fixed = TRUE)
  splineEst <- Xp[, selectedCol] %*% gamFit$coefficients[selectedCol]
  
  criticalValue <- 2
  
  # get the size of the stError based on the simulations
  if(simultaneousCI){
    predictions <- predict(gamFit, 
                           newData, 
                           se.fit = TRUE)
    fitSE <- predictions$se.fit
    
    N <- 10000
    normalSamples <- mgcv::rmvn(N,
                                mu = rep(0, nrow(coVarMatrix)), 
                                V = coVarMatrix)
    Cg <- predict(gamFit, 
                  newData, 
                  type = "lpmatrix")
    simulatedDiff <- Cg %*% t(normalSamples)
    absoluteDiff <- abs(sweep(simulatedDiff, 1, fitSE, FUN = "/"))
    maxDiff <- apply(absoluteDiff, 2L, max)
    criticalValue <- quantile(maxDiff, 
                              prob = 0.95, 
                              type = 8)
  }
  
  upperBound <- splineEst + criticalValue*stError
  lowerBound <- splineEst - criticalValue*stError
    
  final_data <- as.data.table(data.frame(xVar = newDataVar, 
                                         yVar = splineEst, 
                                         upperBound = upperBound, 
                                         lowerBound = lowerBound))
  
  return(final_data)
}
