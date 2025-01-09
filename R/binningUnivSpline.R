#' Split the GAM samples to the best splits based on a tree-model of the `rpart` package. 
#' Bootstrap will be used for the sampling of the trees. 
#' For the final breaks are the mediods of all samples, using the function [cluster::pam()]
#'
#' @param inputDT data.table object with columns y and x for the tree-model. 
#' y is the target variable and x the variable which needs to be splitted
#' @param nGroups positive integer, number of groups that needs to be created
#' @param sampleSizeBin positive integer, datasize used for the tree-model
#' @param nSamples positive integer, number of bootstrapped samples that needs to be created 
#' @param maxIter positive integer, maximum number of iterations for the tree model. 
#' The complexity parameter `cp` from the `rpart` package will be used to have the proper number of groups
#'
#'
#' @importFrom cluster pam
#' @importFrom rpart rpart
#'
#' @returns list of the final break points 
#' @export
binningUnivSpline <- function(inputDT, 
                              nGroups,
                              sampleSizeBin = min(nrow(inputDT), 50000), 
                              nSamples = 10, 
                              maxIter = 500){

  checkDT(inputDT, c('y', 'x'))
  if (is.null(nGroups)) {
    stop("A value needs to be passed to the 'nGroups' argument.")
  }
  checkNumVec(as.list(sampleSizeBin, nSamples, maxIter, nGroups))
  
  breakPoints <- matrix(NA, 
                        nrow = nSamples, 
                        ncol = nGroups - 1)
  
  for(iSample in 1:nSamples){
    selectedIndices <- sample(1:nrow(inputDT), 
                              sampleSizeBin, 
                              replace = TRUE)
    
    complexityParam <- 1
    nIters <- 1
    notDone <- TRUE
    
    while(notDone){
      resultTree <- rpart(y ~ x, 
                          data = inputDT[selectedIndices,], 
                          cp = complexityParam)
      
      splitsTree <- resultTree$splits[,4]
      
      if(is.null(splitsTree) | (length(splitsTree) + 1) < nGroups){
        complexityParam <- complexityParam*0.5
      } else if((length(splitsTree) + 1) > nGroups){
        complexityParam <- complexityParam*1.5
      } else{
        notDone <- FALSE
      }
      
      if(nIters == maxIter){
        notDone <- FALSE
      } 
      nIters <- nIters + 1
    }
    
    if (nIters <= maxIter) {
      breakPoints[iSample, ] <- splitsTree
    }
  }
  
  if (sum(is.na(breakPoints)) == prod(dim(breakPoints))) {
    finalBreakPoints <- NA
  } else {
    rows2Keep <- which(aaply(breakPoints, 1, function(xx) sum(is.na(xx)) == 0))
    finalBreakPoints <- cluster::pam(breakPoints[rows2Keep,], 1)$medoids
  }
  return(as.numeric(finalBreakPoints))
}
