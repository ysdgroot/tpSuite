#TODO: document

binningUnivSpline <- function(inputDT, nGroups = NULL, sampleSizeBin = min(nrow(inputDT), 50000), nSamples = 10, maxIter = 500){
  
  checkDT(inputDT, c('y', 'x'))
  if(is.null(nGroups)) stop("A value needs to be passed to the 'nGroups' argument.")
  checkNumVec(as.list(sampleSizeBin, nSamples, maxIter))
  
  breakPoints <- matrix(NA, nrow = nSamples, ncol = nGroups - 1)
  for(iSample in 1:nSamples){
    selectedIndices <- sample(1:nrow(inputDT), sampleSizeBin)
    complexityParam <- 1
    nIters <- 1
    notDone <- TRUE
    while(notDone){
      resultTree <- rpart(y ~ x, data = inputDT[selectedIndices,], cp = complexityParam)
      splitsTree <- sort(unique(unlist(rpart.lists(resultTree))))
      if(is.character(splitsTree) | (length(splitsTree) + 1) < nGroups){
        complexityParam <- complexityParam*0.5
      } else if((length(splitsTree) + 1) > nGroups){
        complexityParam <- complexityParam*1.5
      } else{
        notDone <- FALSE
      }
      if(nIters == maxIter) notDone <- FALSE
      nIters <- nIters + 1
    }
    if(nIters <= maxIter) breakPoints[iSample, ] <- splitsTree
  }
  library(cluster)
  if(sum(is.na(breakPoints)) == prod(dim(breakPoints))){
    finalBreakPoints <- NA
  } else {
    rows2Keep <- which(aaply(breakPoints, 1, function(xx) sum(is.na(xx)) == 0))
    finalBreakPoints <- pam(breakPoints[rows2Keep,], 1)$medoids
  }
  return(as.numeric(finalBreakPoints))
}
