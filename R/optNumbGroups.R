#' Find the optimal number of splits using a GAM model as a base
#'
#' @param inputDT data.table used for the GAM model to fit
#' @param gamFit fitted GAM model
#' @param splineEst list of the estimated spline models
#' @param splineVarNames list of (continuous) variable names which should be binned. 
#' all the names should be found in the `names(splineEst)` and in  `inputDT`  
#' @param nGroups list of integers, 
#' the different sizes to look for the binning. 
#' @param typeSplit "tree" or "classInt"
#' "tree" being the split based on the trees
#' "classInt" using the [classIntervals()] function to split. 
#' a vector can also be given. 
#' The length should be the same asl the length of `splineVarNames`. 
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
#' @param style see `style` in function [classIntervals()].
#' Default being `fisher`
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
                          typeSplit = "tree", 
                          sampleSizeBin = min(50000, nrow(inputDT)), 
                          minIncr = 0.05, 
                          minIncrRel = 1.05, 
                          minExplVar = 0.6, 
                          verbose = TRUE, 
                          nSamples = 10, 
                          maxIter = 500, 
                          invLink = exp, 
                          style = "fisher"){
  
  # Checks ------------------------------------------------------------------

  
  if (!all(typeSplit %in% c("tree", "classInt"))){
    stop("typeSplit can be 'tree' or 'classInt' ")
  }
  
  if (length(typeSplit) > 1 & length(typeSplit) != length(splineVarNames)) {
    stop("Length of 'typeSplit' should be 1 or the same as 'splineVarNames' ")
  }
  
  checkDT(inputDT, splineVarNames)
  if (!any(c('gam', 'bam') %in% class(gamFit))) {
    stop('The "gamFit" argument should be an output object of the mgcv::gam function.')
  }
  
  allSplineVar <- unlist(llply(gamFit$smooth, 
                               function(xx) xx$term))
  
  for(elementSplineEst in names(splineEst)){
    checkValues(list(elementSplineEst), list(allSplineVar))
  } 
  
  checkCharVec(list(splineVarNames))
  if(length(splineVarNames)){
    splineVarNamesRed <- splineVarNames
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
  
  if (length(typeSplit) == 1) {
    typeSplit <- rep(typeSplit, 
                     length(splineVarNames))
  }

  splittingPoints <- list()
  length(splittingPoints) <- length(splineVarNames)
  names(splittingPoints) <- splineVarNames
  
  explVarList <- list()
  length(explVarList) <- length(splineVarNames)
  names(explVarList) <- splineVarNames
  
  # loop through all the spline variables
  for (iSpline in 1:length(splineVarNames)){
    var2BeGrouped <- splineVarNames[[iSpline]]
    typeSplit_spline <- typeSplit[[iSpline]]
    
    if (verbose) {
      cat(sprintf("%d : %s - Split Type: %s \n", 
                  iSpline, 
                  splineVarNames[iSpline],
                  typeSplit_spline))
    }
    
    splittingPointsTemp <- list()
    iRun <- 1
    explVar <- rep(NA, length(nGroups))
    
    # running the code for each group 
    # Calculating the Explain Variance for each group
    for (iGroup in nGroups) {
      if (verbose) {
        cat(sprintf("\t Group %d \n", 
                    iGroup))
      } 
      splits <- list()
      tempDT <- copy(inputDT)
      
      if (typeSplit_spline == "tree") {
        # data prep for the splits
        inputSplit <- tempDT[as.integer(rownames(splineEst[[var2BeGrouped]])), ]
        inputSplit <- inputSplit[, .SD, 
                                 .SDcol = var2BeGrouped]
        names(inputSplit) <- 'x'
        inputSplit[, y := invLink(splineEst[[var2BeGrouped]])]
        
        # getting the splits 
        splitsTree <- binningUnivSplineTree(inputDT = inputSplit, 
                                            nGroups = iGroup, 
                                            sampleSizeBin = min(nrow(tempDT), sampleSizeBin), 
                                            nSamples = nSamples, 
                                            maxIter = maxIter)
        
        # save the splits 
        splittingPointsTemp[[iRun]] <- splitsTree
        
        splits <- list(splittingPointsTemp[[iRun]])
        names(splits) <- var2BeGrouped
        
        # add a Grouped columns for the variable
        transform2BinnedVar(inputDT = tempDT, 
                            splits = splits)
        
        groupedVarName <- sprintf("%sGrouped", splineVarNames[iSpline])
        
      } else if (typeSplit_spline == "classInt") {
        
        splitsClassInt <- binningUnivSplineClassInterval(splineEst[[var2BeGrouped]], 
                                                         nGroups = iGroup, 
                                                         sampleSizeBin = min(nrow(tempDT), sampleSizeBin), 
                                                         nSamples = nSamples, 
                                                         verbose = FALSE, 
                                                         style = style)
        
        # save the splits 
        colName_spline <- sprintf("%sSpline", 
                                  var2BeGrouped)
        
        splittingPointsTemp[[iRun]] <- splitsClassInt
        splits <- list(splittingPointsTemp[[iRun]])
        
        names(splits) <- colName_spline
        names(splittingPoints)[iSpline] <- colName_spline
        
        tempDT[, (colName_spline) := splineEst[[var2BeGrouped]]]
        
        # add a Grouped columns for the variable
        transform2BinnedVar(inputDT = tempDT, 
                            splits = splits)
        groupedVarName <- sprintf("%sGrouped", colName_spline)
      }
      
      # Construct the explained variance for the variable 
      # only if none are NA
      if (all(!is.na(splits))) {

        explVariance <- calculateExplainedVariance(tempDT, 
                                   splits = splits, 
                                   splineEst = splineEst, 
                                   varName = splineVarNames[iSpline], 
                                   groupedVarName = groupedVarName)
        
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

