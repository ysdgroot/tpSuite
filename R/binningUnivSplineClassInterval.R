#' Get the spline splits based on the specific style of splitting based on the [classInt] package.
#'
#' @param splineEst number with the spline estimates of rht 
#' @param nGroups positive integer, 
#' The number of splits in the y-direction. 
#' Note that this can result in more groups if you want to split in the x-direction
#' @param sampleSizeBin positive integer, 
#' the number of sample to be taken for the splits
#' @param nSamples positive integer, 
#' the number of random samples that needs to be taken for the splits
#' @param verbose `logical(1)` if the status should be shown or not
#' @param style style for the function [classIntervals()]
#'
#' @returns splits in the y-direction
#' @export
#' 
#' @importFrom cluster pam
#'
#' @examples
binningUnivSplineClassInterval <-  function(splineEst, 
                                            nGroups = NULL, 
                                            sampleSizeBin = 50000, 
                                            nSamples = 10, 
                                            verbose = TRUE, 
                                            style = "fisher"){
  
  if(is.null(nGroups)){
    stop("A value needs to be passed to the 'nGroups' argument.")
  } 
  checkNumVec(as.list(splineEst, sampleSizeBin, nSamples))
  checkLength(list(nGroups), 1)
  sampleSizeBin <- min(length(splineEst), sampleSizeBin)
  
  breakPoints <- matrix(NA, 
                        nrow = nSamples, 
                        ncol = nGroups - 1)
  
  for (iSample in 1:nSamples) {
    if (verbose) {
      print(sprintf('Run %d', iSample))
    }
    selectedIndices <- sample(1:length(splineEst), 
                              sampleSizeBin)
    
    classIntResult <- classIntervals(splineEst[selectedIndices], 
                                   n = nGroups, 
                                   style = style)
    
    breakPoints[iSample, ] <- classIntResult$brks[-c(1, length(classIntResult$brks))]
  }
  finalBreakPoints <- pam(breakPoints, 1)$medoids
  
  return(as.numeric(finalBreakPoints))
}
