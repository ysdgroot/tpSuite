#TODO: document

groupingVars <- function(var2BeGrouped, 
                         nGroups, 
                         gamFit, 
                         inputDT, 
                         splineEst, 
                         sampleSizeBin = min(50000, nrow(inputDT)), 
                         nSamples = 10, 
                         maxIter = 500, 
                         xLimit = NULL, 
                         showPlot = FALSE, 
                         plotDir = NULL, 
                         plotName = NULL, 
                         pdf2 = TRUE){
  
  # Begin checks
  checkLength(list(var2BeGrouped, sampleSizeBin, showPlot, pdf2), 1)
  if(!is.null(xLimit)){
    checkLength(list(xLimit), 2)
    checkNumOrIntVec(list(xLimit))
  }
  
  checkCharVec(list(var2BeGrouped))
  checkDT(inputDT, var2BeGrouped)
  checkPlotDirAndName(plotDir, plotName)
  checkLogicVec(list(showPlot, pdf2))
  checkNumOrIntVec(list(nGroups, sampleSizeBin, nSamples, maxIter))
  
  checkRanges(list(nGroups), list(c('>', 1)))
  checkRanges(list(sampleSizeBin), list(c('>', 1)))
  checkWholeNumb(list(nGroups, sampleSizeBin, nSamples, maxIter))
  
  if(sum(class(gamFit) %in% c('gam', 'bam')) == 0){
    stop('The "gamFit" argument should be an output object of the mgcv::[g,b]am function.')
  } 
  
  # End Checks
  
  allSplineVar <- unlist(llply(gamFit$smooth, function(xx) xx$term))
  
  for(elementSplineEst in names(splineEst)) {
    checkValues(list(elementSplineEst), list(allSplineVar))
  }
  checkValues(list(var2BeGrouped), list(allSplineVar))
  checkValues(list(var2BeGrouped), list(names(splineEst)))
  
  #inputSplit <- extractData4ModelFit(inputDT, var2BeGrouped, NULL, NULL)
  inputSplit <- inputDT[as.integer(rownames(splineEst[[which(names(splineEst) == var2BeGrouped)]])), ]
  inputSplit <- inputSplit[, .SD, 
                           .SDcol = which(names(inputSplit) == var2BeGrouped)]
  names(inputSplit) <- 'x'
  
  inputSplit[, y := exp(splineEst[[which(names(splineEst) == var2BeGrouped)]])]
  splitsTree <- binningUnivSpline(inputSplit, 
                                  nGroups, 
                                  sampleSizeBin = sampleSizeBin, 
                                  nSamples = nSamples, 
                                  maxIter = maxIter)
  
  if(!sum(is.na(splitsTree))){
    #library(ggplot2)
    ggplotObject <- plotUnivSpline(gamFit, 
                                   inputDT, 
                                   var2BeGrouped, 
                                   simultaneousCI = FALSE, 
                                   transparence = 0.5, 
                                   colorLine = 'black', 
                                   colorRibbon = 'steelblue', 
                                   lineWidth = 1.2, 
                                   showPlot = TRUE, 
                                   nSamples = 1000, 
                                   xLimits = xLimit)
    ggplotObject <- ggplotObject[[1]] + 
      geom_vline(xintercept = splitsTree, 
                 color = 'forestgreen', 
                 size = 1.1)
    
    plotName <- sprintf("binned %s with %d groups", 
                        var2BeGrouped, 
                        nGroups)
    
    if(showPlot == TRUE){
      print(ggplotObject)
    } 
    
    if(!is.null(plotDir)){
      if(pdf2) {
        ggsave(paste(plotDir, 
                     paste(plotName, '.pdf', sep = ''), 
                     sep = ""))
      }
      ggsave(paste(plotDir, 
                   paste(plotName, '.png', sep = ''), 
                   sep = ""))
    }
    return(list(ggplotObject = ggplotObject, splits = splitsTree))
  } else {
    message <- sprintf('Variable %s could not be split into %d groups.', 
                       var2BeGrouped, 
                       nGroups)
    warning(message)
  }
}
