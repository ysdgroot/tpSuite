#TODO: document

binningSpatialEffect <- function(inputDT, gamFit, nGroups = 5, dirShape = NULL, keyWordColSelection = NULL, sampleSizeBin = min(nrow(inputDT), 50000), nSamples = 10, verbose = FALSE, asList = TRUE, addVar = FALSE){
  
  checkDT(inputDT)
  if(sum(class(gamFit) %in% c('gam', 'bam')) == 0) stop('The "gamFit" argument should be an output object of the mgcv::gam function.')
  checkLength(list(nGroups), 1)
  for(iGroup in nGroups) checkRanges(list(iGroup), list(c('>', 0)))
  checkWholeNumb(list(nGroups, sampleSizeBin))
  
  if(is.null(keyWordColSelection)) keyWordColSelection <- 'latitude'
  if(sum(names(inputDT) %in% c('latitude', 'longitude')) != 2){
    addCoordinates(inputDT, dirShape = dirShape)
  }
  splineEst <- extractSplineEstimate(gamFit, keyWordColSelection)
  splineEst <- unlist(splineEst[which(names(splineEst) == keyWordColSelection)])
  splitsSpatial <- groupingSpatialSpline(splineEst = splineEst, nGroups = nGroups, sampleSizeBin = sampleSizeBin, nSamples = nSamples, verbose = verbose, asList = TRUE) #classicTP
  inputDT[, zone := splineEst]
  if(!addVar){
    transform2BinnedVar(inputDT, splitsSpatial)
  }
  return(list(splits = splitsSpatial))
}
