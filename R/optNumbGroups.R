#' Find the optimal number of splits using a GAM model as a base
#'
#' @param inputDT data.table used for the GAM model to fit
#' @param gamFit fitted GAM model
#' @param splineEst list of the estimated spline models
#' @param splineVarNames list of (continuous) variable names which should be binned. 
#' all the names should be found in the `names(splineEst)` and in  `inputDT`  
#' @param nGroups list of integers, 
#' the different sizes to look for the binning. 
#' @param sampleSizeBin integer, 
#' sample size which should be used for the binning strategy of a tree
#' @param minExplVar single numeric value, 
#' what the minimum explained variance should be. 
#' If the minimum explained variance is not met a warning will be given
#' @param verbose logical, 
#' if current status of the modeling should be shown 
#' @param nSamples integer, 
#' number of different trees that needs to be constructed to find the best splits 
#' @param maxIter maximum number of iterations for the tree construction
#' @param invLink function, the inverse of the link function. 
#' Usually this is the function [exp()]
#' 
#' @inheritParams getBestnGroups
#'
#' @returns list with the explained variance and the best splits for each variable given. 
#' @export
optNumbGroups <- function(inputDT, 
                          gamFit, 
                          splineEst, 
                          splineVarNames, 
                          nGroups = seq(4, 10, 2), 
                          sampleSizeBin = min(50000, nrow(inputDT)), 
                          minIncr = 0.05, 
                          minIncrRel = 1.05, 
                          minExplVar = 0.6, 
                          verbose = TRUE, 
                          nSamples = 10, 
                          maxIter = 500, 
                          invLink = exp){
  
  # Checks ------------------------------------------------------------------

  
  checkDT(inputDT, splineVarNames[splineVarNames != 'zone'])
  if (!any(c('gam', 'bam') %in% class(gamFit))) {
    stop('The "gamFit" argument should be an output object of the mgcv::gam function.')
  }
  
  allSplineVar <- unlist(llply(gamFit$smooth, 
                               function(xx) xx$term))
  
  for(elementSplineEst in names(splineEst)){
    checkValues(list(elementSplineEst), list(allSplineVar))
  } 
  
  checkCharVec(list(splineVarNames))
  if(length(splineVarNames[splineVarNames != 'zone'])){
    splineVarNamesRed <- splineVarNames[splineVarNames != 'zone']
    for (iCheck in 1:length(splineVarNamesRed)) {
      checkValues(list(splineVarNamesRed[iCheck]), list(names(splineEst)))
    } 
  }
  
  checkNumOrIntVec(list(nGroups, sampleSizeBin, nSamples, maxIter))
  for(iGroup in nGroups){
    checkRanges(list(iGroup), list(c('>', 0)))
  } 
  checkWholeNumb(list(nGroups, sampleSizeBin, nSamples, maxIter))
  
  checkLength(list(minIncr, minIncrRel, sampleSizeBin), 1)
  checkNumVec(list(minIncr, minIncrRel))
  checkRanges(list(minIncr), list(c('>=', 0, '<=', 1)))
  
  checkRanges(list(minIncrRel), list(c('>=', 1)))
  
  # End Checks --------------------------------------------------------------
  # -------------------------------------------------------------------------

  splittingPoints <- list()
  length(splittingPoints) <- length(splineVarNames)
  names(splittingPoints) <- splineVarNames
  
  explVarList <- list()
  length(explVarList) <- length(splineVarNames)
  names(explVarList) <- splineVarNames
  
  # loop through all the spline variables
  for (iSpline in 1:length(splineVarNames)){
    var2BeGrouped <- splineVarNames[[iSpline]]
    
    if (verbose) {
      cat(sprintf("%d : %s \n", 
                  iSpline, 
                  splineVarNames[iSpline]))
    }
    
    splittingPointsTemp <- list()
    iRun <- 1
    explVar <- rep(NA, length(nGroups))
    
    # running the code for each group 
    # Calculating the Explain Variance for each group
    for(iGroup in nGroups){
      if(verbose){
        cat(sprintf("\t Group %d \n", 
                    iGroup))
      } 
      splits <- list()
      tempDT <- copy(inputDT)
      
      if(splineVarNames[iSpline] %in% c('longitude', 'latitude', 'zone')){
        splittingPointsTemp[[iRun]] <- groupingSpatialSpline(splineEst = splineEst[which(names(splineEst) %in% c('longitude', 'latitude'))][[1]], 
                                                             nGroups = iGroup, 
                                                             sampleSizeBin = min(nrow(tempDT), sampleSizeBin), 
                                                             nSamples = nSamples, 
                                                             verbose = FALSE, 
                                                             asList = FALSE) #classicTP
        splits[[1]] <- splittingPointsTemp[[iRun]]
        inputDT[, zone := splineEst[which(names(splineEst) %in% c('longitude', 'latitude'))][[1]]]
        tempDT[, zone := splineEst[which(names(splineEst) %in% c('longitude', 'latitude'))][[1]]]
        names(splits) <- 'zone'
        groupedVarName <- 'zoneGrouped'
        names(splittingPoints)[iSpline] <- 'zone'
      } 
      else {
        
        # data prep for the splits
        inputSplit <- tempDT[as.integer(rownames(splineEst[[var2BeGrouped]])), ]
        inputSplit <- inputSplit[, .SD, 
                                 .SDcol = var2BeGrouped]
        names(inputSplit) <- 'x'
        inputSplit[, y := invLink(splineEst[[var2BeGrouped]])]
        
        # getting the splits 
        splitsTree <- binningUnivSpline(inputDT = inputSplit, 
                                        nGroups = iGroup, 
                                        sampleSizeBin = sampleSizeBin, 
                                        nSamples = nSamples, 
                                        maxIter = maxIter)
        
        # save the splits 
        splittingPointsTemp[[iRun]] <- splitsTree
        
        splits <- list(splittingPointsTemp[[iRun]])
      }
      
      # Construct the explained variance for the variable 
      # only if none are NA
      if (all(!is.na(splits))) {
        names(splits) <- var2BeGrouped
        explVariance <- calculateExplainedVariance(tempDT, 
                                   splits = splits, 
                                   splineEst = splineEst, 
                                   varName = splineVarNames[iSpline], 
                                   groupedVarName = sprintf("%sGrouped", splineVarNames[iSpline]))
        
        explVar[iRun] <- explVariance
      }
      
      iRun <- iRun + 1
    } # end of a group size calculation
    
    if (all(is.na(explVar))) {
      splittingPoints[[iSpline]] <- NA
      explVarList[[iSpline]] <- NA
      warning(sprintf("No valid splits for any of the tested groups (cfr. 'nGroups' argument) were found for the variable %s.", 
                      splineVarNames[iSpline]))
    } else {
      
      selInds <- !is.na(explVar)
      
      explVarSub <- explVar[selInds]
      nGroupsSub <- nGroups[selInds]
      
      bestExplainedVariance <- getBestnGroups(explVar[selInds], 
                                              nGroups[selInds], 
                                              splittingPointsTemp[selInds])
      
      splittingPoints[[iSpline]] <- bestExplainedVariance$SplittingPoints
      explVarList[[iSpline]] <- bestExplainedVariance$ExplainedVariance
      
      if (bestExplainedVariance$ExplainedVariance < minExplVar) {
        warning(sprintf("Even the optimal grouping for the variable %s 
                           did not exceed the minimal value for explained variance, 
                           as set by the 'minExplVar' argument \n 
                           Please increase the number of possible groups, 
                           as indicated by the 'nGroups' argument.",
                        splineVarNames[iSpline]))
      }
    } # end of explained variance checks
  } # end of spline calculations
  
  return(list(splits = splittingPoints, 
              explVar = explVarList))

}

