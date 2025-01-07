plotUnivSpline <- function(gamFit, inputDT, selectedVar, simultaneousCI = FALSE, transparence = 0.5, colorLine = 'black', colorRibbon = 'steelblue', lineWidth = 1.2, showPlot = TRUE, nSamples = 1000, plotDir = NULL, plotName = NULL, pdf2 = TRUE, xLimits = NULL, yLimits = NULL){
  
  checkPlotDirAndName(plotDir, plotName)
  
  if(sum(class(gamFit) %in% 'gam') == 0) stop('The "gamFit" argument should be an output object of the mgcv::gam function.')
  if(!is.data.table(inputDT)) stop('The "inputDT" argument should be a data.table object.')
  if(length(selectedVar) < 1 & selectedVar %in% names(inputDT) != length(selectedVar)) stop('The "selectedVar" argument should be a character vector consisting of column names of the "inputDT" argument.')
  
  if(!is.null(xLimits)){
    if(length(xLimits) != 2 | !is.numeric(xLimits)) stop('The "xLimits" argument should be a numeric vector of length 2.')
  }
  if(!is.null(yLimits)){
    if(length(yLimits) != 2 | !is.numeric(yLimits)) stop('The "yLimits" argument should be a numeric vector of length 2.')
  }
  
  for(iVar in 1:length(selectedVar)){
    varNumber <- 1
    goOn <- TRUE
    while(goOn){
      modelVar <- try(attr(gamFit$terms[varNumber], 'term.labels'), silent = TRUE)
      if(class(modelVar) == "try-error") stop(paste(selectedVar[iVar], 'is not part of the model variables of the "gamFit" argument.'))
      if(selectedVar[iVar] == modelVar) goOn <- FALSE
      varNumber <- varNumber + 1
    }
  }
  
  includedVars <- attr(gamFit$terms, 'dataClasses')
  numericVarNames <- names(includedVars)[which(includedVars == 'numeric')]
  numericVarNames <- numericVarNames[!grepl('offset', numericVarNames)]
  categoricalVarNames <- names(gamFit$xlevels)
  
  referenceClass <- laply(gamFit$xlevels, function(xx) xx[1])
  refDF <- data.frame(t(c(1, rep(0, length(numericVarNames)), referenceClass)))
  names(refDF) <- c('exposure', numericVarNames, categoricalVarNames)
  
  ggplotObject <- list()
  coVarMatrix <- vcov(gamFit)
  N <- 10000
  
  for(iVar in 1:length(selectedVar)){
    
    newDataVar <- seq(min(inputDT[[which(names(inputDT) == selectedVar)]], na.rm = TRUE), max(inputDT[[which(names(inputDT) == selectedVar)]], na.rm = TRUE), length = nSamples)
    newData <- data.frame(refDF, temp = rep(-1, length(newDataVar)))
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
    
    if(simultaneousCI == TRUE){
      
      predictions <- predict(gamFit, newData, se.fit = TRUE)
      fitSE <- predictions$se.fit
      
      normalSamples <- rMVN(N, mu = rep(0, nrow(coVarMatrix)), sig = coVarMatrix)
      Cg <- predict(gamFit, newData, type = "lpmatrix")
      simulatedDiff <- Cg %*% t(normalSamples)
      absoluteDiff <- abs(sweep(simulatedDiff, 1, fitSE, FUN = "/"))
      maxDiff <- apply(absoluteDiff, 2L, max)
      criticalValue <- quantile(maxDiff, prob = 0.95, type = 8)
      
      upperBound <- splineEst + criticalValue*stError
      lowerBound <- splineEst - criticalValue*stError
      
    } else {
      
      predictions <- predict(gamFit, newData, type = "terms", se.fit = TRUE)
      fitSE <- predictions$se.fit
      
      selectedVarSpline <- paste(paste('s(', selectedVar, sep = ''), ')', sep = '')
      selectedCol <- grep(selectedVarSpline, colnames(fitSE), fixed = TRUE)
      stError <- fitSE[, selectedCol]
      
      Xp <- predict(gamFit, newdata = newData, type = "lpmatrix")
      
      selectedCol <- grep(selectedVarSpline, colnames(Xp), fixed = TRUE)
      splineEst <- Xp[, selectedCol]%*%gamFit$coefficients[selectedCol]
      
      upperBound <- splineEst + 2*stError
      lowerBound <- splineEst - 2*stError
      
    }
    ggplotData <- as.data.table(data.frame(xVar = newDataVar, yVar = splineEst, upperBound = upperBound, lowerBound = lowerBound))
    
    if(is.null(xLimits) & is.null(yLimits)){
      ggplotObject[[iVar]] <- ggplot(ggplotData, aes(x = xVar, y = yVar)) + geom_line(lwd = lineWidth, color = colorLine) +
        geom_ribbon(aes(ymin = lowerBound, ymax = upperBound), alpha = transparence, fill = colorRibbon) +
        xlab(selectedVar[iVar]) + ylab('Predictions + uncertainty')
    } else if(is.null(xLimits) & !is.null(yLimits)){
      ggplotObject[[iVar]] <- ggplot(ggplotData, aes(x = xVar, y = yVar)) + geom_line(lwd = lineWidth, color = colorLine) +
        geom_ribbon(aes(ymin = lowerBound, ymax = upperBound), alpha = transparence, fill = colorRibbon) +
        xlab(selectedVar[iVar]) + ylab('Predictions + uncertainty') + coord_cartesian(ylim = yLimits)
    } else if(!is.null(xLimits) & is.null(yLimits)){
      ggplotObject[[iVar]] <- ggplot(ggplotData, aes(x = xVar, y = yVar)) + geom_line(lwd = lineWidth, color = colorLine) +
        geom_ribbon(aes(ymin = lowerBound, ymax = upperBound), alpha = transparence, fill = colorRibbon) +
        xlab(selectedVar[iVar]) + ylab('Predictions + uncertainty') + coord_cartesian(xlim = xLimits)
    } else if(!is.null(xLimits) & !is.null(yLimits)){
      ggplotObject[[iVar]] <- ggplot(ggplotData, aes(x = xVar, y = yVar)) + geom_line(lwd = lineWidth, color = colorLine) +
        geom_ribbon(aes(ymin = lowerBound, ymax = upperBound), alpha = transparence, fill = colorRibbon) +
        xlab(selectedVar[iVar]) + ylab('Predictions + uncertainty') + coord_cartesian(xlim = xLimits) + coord_cartesian(ylim = yLimits)
    }
    if(showPlot == TRUE) print(ggplotObject[[iVar]])
    if(!is.null(plotDir) & !is.null(plotName)){
      if(pdf2) ggsave(paste(plotDir, paste(plotName, '.pdf', sep = ''), sep = ""))
      ggsave(paste(plotDir, paste(plotName, '.png', sep = ''), sep = ""))
    }
  }
  return(ggplotObject)
}

