#' Plot the univariate spline of a GAM model
#'
#' @param gamFit fitted GAM model, from the package [mgcv]
#' @param inputDT data.table object which is used to train the GAM model 
#' @param selectedVar single character variable
#' @param simultaneousCI logical, if the CI should be simulated
#' @param transparence alpha value for the ribbon, see [ggplot::geom_ribbon]
#' @param colorLine colorname or hexcode for the splice
#' @param colorRibbon colorname or hexcode for the ribbon
#' @param lineWidth linewidth for the spline
#' @param showPlot logical, if the plot should be shown or not
#' @param nSamples numeric, number of points to be used to extract spline
#' @param plotDir path location for the directory
#' @param plotName name of the plot, will be saved at location `plotDir`
#' @param pdf2 logical, if the plot should be saved as a pdf
#' @param xLimits,yLimits numeric of length 2, indicating the limits of the plots
#'
#' @importFrom mgcv rmvn
#'
#' @returns ggplot object of the spline
#' @export
plotUnivSpline <- function(gamFit, 
                           selectedVar, 
                           min_max_values, 
                           simultaneousCI = FALSE,
                           nSamples = 1000, 
                           transparence = 0.5, 
                           colorLine = 'black', 
                           colorRibbon = 'steelblue', 
                           lineWidth = 1.2, 
                           showPlot = TRUE, 
                           plotDir = NULL, 
                           plotName = NULL, 
                           pdf2 = TRUE, 
                           xLimits = NULL, 
                           yLimits = NULL){
  
  # checks
  
  checkPlotDirAndName(plotDir, plotName)

  if (!is.null(xLimits)) {
    if (length(xLimits) != 2 | !is.numeric(xLimits)) {
      stop('The "xLimits" argument should be a numeric vector of length 2.')
    } 
  }
  if (!is.null(yLimits)) {
    if (length(yLimits) != 2 | !is.numeric(yLimits)) {
      stop('The "yLimits" argument should be a numeric vector of length 2.')
    } 
  }
  
  # get the spline extraction with the CI of the spline 
  ggplotData <- splineExtract(gamFit = gamFit, 
                              selectedVar = selectedVar, 
                              min_max_values = min_max_values, 
                              nSamples = nSamples, 
                              simultaneousCI = simultaneousCI)
  
  ggplot_p <- ggplot(ggplotData, 
                     aes(x = xVar, 
                         y = yVar)) + 
    geom_line(lwd = lineWidth, 
              color = colorLine) +
    geom_ribbon(aes(ymin = lowerBound, 
                    ymax = upperBound), 
                alpha = transparence, 
                fill = colorRibbon) +
    xlab(selectedVar) + 
    ylab('Predictions + uncertainty')
  
  if (!is.null(xLimits)) {
    ggplot_p <- ggplot_p +
      coord_cartesian(xlim = xLimits)
  }
  
  if (!is.null(yLimits)) {
    ggplot_p <- ggplot_p + 
      coord_cartesian(ylim = yLimits)
  }
  
  if (showPlot == TRUE) {
    print(ggplot_p)
  }
  
  if (!is.null(plotDir) & !is.null(plotName)){
    if (pdf2) ggsave(paste(plotDir, paste(plotName, '.pdf', sep = ''), sep = ""))
    ggsave(paste(plotDir, paste(plotName, '.png', sep = ''), sep = ""))
  }
    
  return(ggplot_p)
}

