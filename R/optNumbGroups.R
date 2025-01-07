#TODO: document


optNumbGroups <- function(inputDT, gamFit, splineEst, splineVarNames, nGroups = seq(4, 10, 2), plotDir = NULL, sampleSizeBin = min(50000, nrow(inputDT)), minIncr = 0.05, minIncrRel = 1.05, showExplVar = FALSE, minExplVar = 0.6, addVar = FALSE, verbose = TRUE, nSamples = 10, maxIter = 500, xLimit = NULL){
  
  checkDT(inputDT, splineVarNames[splineVarNames != 'zone'])
  if(sum(class(gamFit) %in% c('gam', 'bam')) == 0) stop('The "gamFit" argument should be an output object of the mgcv::gam function.')
  allSplineVar <- unlist(llply(gamFit$smooth, function(xx) xx$term))
  for(elementSplineEst in names(splineEst)) checkValues(list(elementSplineEst), list(allSplineVar))
  
  checkCharVec(list(splineVarNames))
  if(length(splineVarNames[splineVarNames != 'zone'])){
    splineVarNamesRed <- splineVarNames[splineVarNames != 'zone']
    for(iCheck in 1:length(splineVarNamesRed)) checkValues(list(splineVarNamesRed[iCheck]), list(names(splineEst)))
  }
  
  checkNumOrIntVec(list(nGroups, sampleSizeBin, nSamples, maxIter))
  for(iGroup in nGroups) checkRanges(list(iGroup), list(c('>', 0)))
  checkWholeNumb(list(nGroups, sampleSizeBin, nSamples, maxIter))
  
  checkLength(list(showExplVar, minIncr, minIncrRel, addVar, sampleSizeBin), 1)
  checkNumVec(list(minIncr, minIncrRel))
  checkRanges(list(minIncr), list(c('>=', 0, '<=', 1)))
  
  checkRanges(list(minIncrRel), list(c('>=', 1)))
  checkLogicVec(list(showExplVar, addVar))
  checkEqualLength(list(splineVarNames, xLimit))
  
  splittingPoints <- list()
  explVarList <- list()
  
  length(splittingPoints) <- length(splineVarNames)
  names(splittingPoints) <- splineVarNames
  length(explVarList) <- length(splineVarNames)
  names(explVarList) <- splineVarNames
  
  for(iSpline in 1:length(splineVarNames)){
    
    if(verbose) print(paste(paste(iSpline, '.: ', sep = ''), splineVarNames[iSpline], sep = ''))
    splittingPointsTemp <- list()
    iRun <- 1
    explVar <- rep(NA, length(nGroups))
    for(iGroup in nGroups){
      if(verbose) print(paste('  Group ', iGroup, sep = ''))
      splits <- list()
      tempDT <- copy(inputDT)
      if(splineVarNames[iSpline] %in% c('longitude', 'latitude', 'zone')){
        splittingPointsTemp[[iRun]] <- groupingSpatialSpline(splineEst = splineEst[which(names(splineEst) %in% c('longitude', 'latitude'))][[1]], nGroups = iGroup, sampleSizeBin = min(nrow(tempDT), sampleSizeBin), nSamples = nSamples, verbose = FALSE, asList = FALSE) #classicTP
        splits[[1]] <- splittingPointsTemp[[iRun]]
        inputDT[, zone := splineEst[which(names(splineEst) %in% c('longitude', 'latitude'))][[1]]]
        tempDT[, zone := splineEst[which(names(splineEst) %in% c('longitude', 'latitude'))][[1]]]
        names(splits) <- 'zone'
        groupedVarName <- 'zoneGrouped'
        names(splittingPoints)[iSpline] <- 'zone'
      } else {
        splittingPointsTemp[[iRun]] <- groupingVars(splineVarNames[iSpline], iGroup, gamFit, tempDT, splineEst, sampleSizeBin = min(nrow(tempDT), sampleSizeBin), nSamples = nSamples, maxIter = maxIter, showPlot = FALSE, xLimit = xLimit[[iSpline]])
        if(is.list(splittingPointsTemp[[iRun]])){
          splits[[1]] <- splittingPointsTemp[[iRun]]$splits
        } else {
          splits[[1]] <- NA
        }
        names(splits) <- splineVarNames[iSpline]
        groupedVarName <- paste(splineVarNames[iSpline], 'Grouped', sep = '')
      }
      
      if(sum(is.na(splits[[1]])) == 0){
        
        transform2BinnedVar(tempDT, splits)
        levs <- extractLevelDT(tempDT, groupedVarName)[[1]]
        sdGroup <- rep(NA, length(levs))
        nObs <- rep(NA, length(levs))
        for(iLev in 1:length(levs)){
          workDT <- copy(tempDT)
          #workDT <- extractData4ModelFit(workDT, splineVarNames[iSpline], NULL, NULL)
          workDT <- workDT[as.integer(rownames(splineEst[[which(names(splineEst) == splineVarNames[iSpline])]])), ]
          if(splineVarNames[iSpline] == 'zone'){
            workDT[, splineEst := splineEst[which(names(splineEst) %in% c('longitude', 'latitude'))][[1]]]
          } else {
            workDT[, splineEst := splineEst[which(names(splineEst) == splineVarNames[iSpline])]]
          }
          splineEstSubset <- subsetDT(workDT, which(names(workDT) == groupedVarName), levs[iLev])$splineEst
          sdGroup[iLev] <- sd(splineEstSubset, na.rm = T)
          nObs[iLev] <- length(splineEstSubset)
        }
        if(splineVarNames[iSpline] == 'zone'){
          explVar[iRun] <- 1 - (sum(sdGroup*nObs)/sum(nObs))/sd(splineEst[which(names(splineEst) %in% c('longitude', 'latitude'))][[1]])
        } else {
          explVar[iRun] <- 1 - (sum(sdGroup*nObs)/sum(nObs))/sd(splineEst[which(names(splineEst) == splineVarNames[iSpline])][[1]])
        }
      }
      iRun <- iRun + 1
    }
    
    if(sum(is.na(explVar)) == length(explVar)){
      splittingPoints[[iSpline]] <- NA
      explVarList[[iSpline]] <- NA
      warning(paste("No valid splits for any of the tested groups (cfr. 'nGroups' argument) were found for the variable ", splineVarNames[iSpline], sep = ''))
    } else {
      
      selInds <- !is.na(explVar)
      explVarSub <- explVar[selInds]
      nGroupsSub <- nGroups[selInds]
      
      if(sum(selInds) > 1){
        selGroup <- which(explVarSub[2:length(nGroupsSub)] - explVarSub[1:(length(nGroupsSub) - 1)] < minIncr)
        selGroupRel <- which(explVarSub[2:length(nGroupsSub)] < minIncrRel* explVarSub[1:(length(nGroupsSub) - 1)])
        
        if(length(selGroup) > 1) selGroup <- max(selGroup)
        if(length(selGroupRel) > 1) selGroupRel <- max(selGroupRel)
        if(length(selGroup) == 0) selGroup <- length(nGroupsSub)
        if(length(selGroupRel) == 0) selGroupRel <- length(nGroupsSub)
        splittingPoints[[iSpline]] <- splittingPointsTemp[[which(selInds)[min(selGroup, selGroupRel)]]]
        explVarList[[iSpline]] <- explVarSub[min(selGroup, selGroupRel)]
        if(explVarSub[min(selGroup, selGroupRel)] < minExplVar) warning(paste(paste(paste(paste('Even the optimal grouping for the variable ', splineVarNames[iSpline], sep = ''), ' did not exceed the minimal value for the explained variance', sep = ''), "as set by the 'minExplVar' argument. Please increase the number of possible groups, as indicated by the 'nGroups' argument.")))
      } else {
        splittingPoints[[iSpline]] <- splittingPointsTemp[[which(selInds)]]
        explVarList[[iSpline]] <- explVarSub[selInds]
        if(explVar[selInds] < minExplVar) warning(paste(paste(paste(paste('Even the optimal grouping for the variable ', splineVarNames[iSpline], sep = ''), ' did not exceed the minimal value for the explained variance', sep = ''), "as set by the 'minExplVar' argument. Please increase the number of possible groups, as indicated by the 'nGroups' argument.")))
      }
    }
    if(!is.null(plotDir)) groupingVars(splineVarNames[iSpline], length(unlist(splittingPoints[[iSpline]])) + 1, gamFit, inputDT, splineEst, plotDir = plotDir, plotName = paste(splineVarNames[iSpline], 'Grouped', sep = ''), sampleSizeBin = min(nrow(inputDT), sampleSizeBin), nSamples = nSamples, maxIter = maxIter, showPlot = FALSE, xLimit = xLimit[[iSpline]])
  }
  
  if(addVar){
    transform2BinnedVar(inputDT, splittingPoints)
  }
  
  if(showExplVar){
    return(list(splits = splittingPoints, explVar = explVarList))
  } else {
    return(list(splits = splittingPoints))
  }
}

