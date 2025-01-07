#TODO: document

groupingSpatialSpline <- function(splineEst, nGroups = NULL, sampleSizeBin = 50000, nSamples = 10, verbose = TRUE, asList = TRUE){
  
  if(is.null(nGroups)) stop("A value needs to be passed to the 'nGroups' argument.")
  checkNumVec(as.list(splineEst, sampleSizeBin, nSamples))
  checkLength(list(nGroups), 1)
  sampleSizeBin = min(length(splineEst), sampleSizeBin)
  
  breakPoints <- matrix(NA, nrow = nSamples, ncol = nGroups - 1)
  for(iSample in 1:nSamples){
    if(verbose) print(paste('Run', iSample, sep = ''))
    selectedIndices <- sample(1:length(splineEst), sampleSizeBin)
    fisherResult <- classIntervals(splineEst[selectedIndices], n = nGroups, style = 'fisher')
    breakPoints[iSample, ] <- fisherResult$brks[-c(1, length(fisherResult$brks))]
  }
  finalBreakPoints <- pam(breakPoints, 1)$medoids
  
  if(asList == TRUE){
    finalResult <- list()
    finalResult[[1]] <- finalBreakPoints
    names(finalResult) <- 'zone'
    return(finalResult)
  } else {
    return(as.numeric(finalBreakPoints))
  }
}
