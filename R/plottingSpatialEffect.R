
#' Constructing a ggplot-styled map of the Belgium, splitted up by INS region, to represent a zone by means of a continuous variable or simply the exposure
#'
#' @param inputDT data.table object containing the data of interest. Note that the 'inputDT' argument needs to contain an 'exposure ' and 'postalCode' column in order to the function to work. This is an obligatory argument, without default value.
#' @param predictions Numeric vector of length corresponding to the number of rows of the 'inputDT' argument. This argument correspond to the predictions (mostly the spline covariate effect estimates) made for every observation of the 'inputDT' argument. In case that the NULL value is supplied to this argument, the exposure is plotted. This is an optional argument, without default value.
#' @param plotTitle Character vector of length 1 consisting of the plot title of the plot. This argument needs to be supplied in conjunction with the 'plotDir' argument. This is an optional argument, with '' as default value.
#' @param dirShape Character vector of length 1 consisting of the directory where one can find the shape files. This is an optional argument, with NULL as default value.
#' @param plotDir Character vector of length 1 consisting of the directory where one wants to save the ggplot object to. This argument needs to be supplied in conjunction with the 'plotName' argument. This is an optional argument, with NULL as default value.
#' @param plotName Character vector of length 1 consisting of the file name of the saved ggplot object to. This argument needs to be supplied in conjunction with the 'plotDir' argument. This is an optional argument, with NULL as default value.
#' @param pdf2 Logical vector of length 1 indicating whether or not one wants to save the ggplot object as a pdf as well, on top of the png version of the ggplot object which is produced in any case. This is an optional argument, with TRUE as default value.
#' @param lowColor Character vector of length 1 indicating the color of the lowest value of the scale of the 'predictions' argument. This argument will only be taken into account if a valid value is also provided for the 'highColor' argument, and when the 'predictions' argument is not considered to be a factor. This is an optional argument, with NULL as default value.
#' @param highColor Character vector of length 1 indicating the color of the highest value of the scale of the 'predictions' argument. This argument will only be taken into account if a valid value is also provided for the 'lowColor' argument, and when the 'predictions' argument is not considered to be a factor. This is an optional argument, with NULL as default value.
#' @param predAsFact Logical vector of length 1 indicating whether or the 'predictions' argument needs to be considered as a continuous or a factor variable. This is an optional argument, with FALSE as default value.
#' @param predFactColors Character vector of length equal to the number of different values of the 'predictions' argument. Only if the 'predAsFact' argument is toggled to TRUE, this argument is taken into account. This is an optional argument, with NULL as default value.
#' @param randomPick Logical vector of length 1 indicating whether one wants to let a random color generator decide upon the color of the different levels of the 'zoneGrouped' factor column of the 'inputDT' argument. If this argument is set to be TRUE, this function will generate an output object that should be used as an input for the first argument of the 'randomColorPick' function (of this package). This is an optional argument, with FALSE as default value.
#' @return A ggplot object is constructed with a continuous color scheme, plotting the exposure or the predictions for the different INS regions. Note that the 'inputDT' argument needs to contain an 'exposure ' and 'postalCode' column in order to the function to work.
#' @examples
#' library(data.table)
#' dirShape <- '/home/robin/shapeFiles/'
#' postalCode <- read.csv(paste(dirShape, "postins.csv", sep = "\\"))[,c("CODPOSS","INS")]
#' inputDT <- data.table(postalCode = postalCode$CODPOSS, exposure = runif(nrow(postalCode)))
#' predictions <- as.numeric(as.character(postalCode$CODPOSS))
#' plottingSpatialEffect(inputDT)
#' plottingSpatialEffect(inputDT, predictions, dirShape = dirShape)
#' plottingSpatialEffect(inputDT, predictions, lowColor = 'red', highColor = 'green', dirShape = dirShape)
#' plottingSpatialEffect(inputDT, predictions, lowColor = 'green', highColor = 'purple', dirShape = dirShape)
#'
#' predictions <- sample(LETTERS[1:10], nrow(postalCode), replace = TRUE)
#' plottingSpatialEffect(inputDT, predictions, predAsFact = TRUE, dirShape = dirShape)
#' @export
plottingSpatialEffect <- function(inputDT, predictions = NULL, plotTitle = '', dirShape = NULL, plotDir = NULL, plotName = NULL, pdf2 = TRUE, lowColor = NULL, highColor = NULL, predAsFact = FALSE, predFactColors = NULL, randomPick = FALSE){
  
  checkDT(inputDT, c('exposure', 'postalCode'))
  checkLength(list(pdf2, randomPick, predAsFact, dirShape), 1)
  if(!is.null(plotTitle)) checkLength(list(plotTitle), 1)
  if(!is.null(dirShape)) checkLength(list(dirShape), 1)
  if(!is.null(plotDir)) checkLength(list(plotDir), 1)
  if(!is.null(plotName)) checkLength(list(plotName), 1)
  checkPlotDirAndName(plotDir, plotName)
  
  if(!is.null(predictions) & !predAsFact) checkNumVec(list(predictions))
  if(!is.null(predictions) & predAsFact) if(!is.factor(predictions)) stop("Please transform argument 'predictions' to a factor.")
  checkCharVec(list(plotTitle, dirShape))
  checkLogicVec(list(pdf2, randomPick, predAsFact))
  
  if(!is.null(highColor)){ checkLength(list(highColor), 1) ; if(sum(highColor %in% colors()) == 0) stop('Please provide a valid color name for the "highColor" argument. The valid values can be found by running the "colors()" function on the command line.')}
  if(!is.null(lowColor)){ checkLength(list(lowColor), 1) ; if(sum(lowColor %in% colors()) == 0) stop('Please provide a valid color name for the "lowColor" argument. The valid values can be found by running the "colors()" function on the command line.')}
  if(!is.null(lowColor) & is.null(highColor)) stop('Both "lowColor" and "highColor" either need to be NULL, either need to have a valid color name. The valid values can be found by running the "colors()" function on the command line.')
  if(is.null(lowColor) & !is.null(highColor)) stop('Both "lowColor" and "highColor" either need to be NULL, either need to have a valid color name. The valid values can be found by running the "colors()" function on the command line.')
  
  lastCharDir <- substr(dirShape, start = nchar(dirShape), stop = nchar(dirShape))
  while(lastCharDir == '\\' | lastCharDir == '/'){
    dirShape <- substr(dirShape, start = 1, stop = nchar(dirShape) - 1)
    lastCharDir <- substr(dirShape, start = nchar(dirShape), stop = nchar(dirShape))
  }
  
  belgium <- readOGR(dsn = dirShape, layer = 'communes_08', verbose = FALSE)
  postalCode <- read.csv(file.path(dirShape, "postins.csv"))[,c("CODPOSS","INS")]
  belgium@data <- left_join(belgium@data, postalCode, by = 'INS')
  
  exposure <- aggregate(exposure ~ postalCode, FUN = sum, data = inputDT)
  exposure$postalCode <- as.numeric(as.character(exposure$postalCode))
  names(belgium@data)[7] <- "postalCode"
  belgium@data <- left_join(belgium@data, exposure, by = 'postalCode')
  for(i in 1:nrow(belgium)) belgium$relativeExposure[i] <- belgium$exposure[i]/sum(belgium$exposure)
  belgiumFortified <- suppressMessages(fortify(belgium))
  belgium$id <- row.names(belgium)
  belgiumFortified <- left_join(belgiumFortified, belgium@data, by = 'id')
  
  if(is.null(predictions)){
    
    mapExposure <- ggplot(belgiumFortified, aes(long, lat, group = group, fill = exposure, text = paste(paste(paste('Postal Code: ', postalCode, sep = ''), ' -- ', sep = ''), Name1, sep = ''))) + xlab('Longitude') + ylab('Latitude') + geom_polygon() +
      ggtitle(plotTitle) + scale_fill_continuous(name = '')
    if(!is.null(lowColor) & !is.null(highColor)){
      mapExposure <- mapExposure + scale_colour_gradient(low = lowColor, high = highColor, name = "")
    } else {
      mapExposure <- mapExposure + scale_colour_gradient(name = "")
    }
    if(!is.null(plotDir) & !is.null(plotName)){
      if(pdf2) ggsave(paste(plotDir, paste(plotName, '.pdf', sep = ''), sep = ""))
      ggsave(paste(plotDir, paste(plotName, '.png', sep = ''), sep = ""))
    }
    return(mapExposure)
  } else {
    
    checkLength(list(predictions), nrow(inputDT))
    if(sum(names(inputDT) == 'predicted') == 1) inputDT[, predicted := NULL] #part of debug
    inputDT$predicted <- predictions
    predicted <- aggregate(predicted ~ postalCode, FUN = getMode, data = inputDT)
    #predicted <- aggregate(predicted ~ postalCode, FUN = mean, data = inputDT)
    predicted$postalCode <- as.numeric(as.character(predicted$postalCode))
    belgiumFortified <- left_join(belgiumFortified, predicted, by = 'postalCode')
    
    if(!predAsFact){
      mapPredictions <- ggplot(belgiumFortified, aes(long, lat, group = group, fill = predicted, text = paste(paste(paste('Postal Code: ', postalCode, sep = ''), ' -- ', sep = ''), Name1, sep = ''))) + xlab('Longitude') + ylab('Latitude') + geom_polygon() + ggtitle(plotTitle)
      if(!is.null(lowColor) & !is.null(highColor)){
        mapPredictions <- mapPredictions + scale_fill_continuous(low = lowColor, high = highColor, name = "")
      } else {
        mapPredictions <- mapPredictions + scale_fill_continuous(name = "")
      }
    } else {
      if(length(unique(predictions)) > 20) warning('The number of unique values of the "predictions" argument is larger than 20. Probably it is not a good idea to treat this variable as a factor.')
      if(!is.null(predFactColors)){
        if(length(unique(predictions)) != length(predFactColors)) stop('The number of colours in the "predFactColors" argument should correspond to the number of different values of the "predictions" argument.')
        if(sum(predFactColors %in% colors()) != length(unique(predictions))) stop('Some of the values of the "predFactColors" argument are not valid color names. The valid values can be found by running the "colors()" function on the command line.')
      } else {
        colorPalette <- colors()[-grep('white', colors())]
        colorPalette <- colorPalette[-grep('grey', colorPalette)]
        colorPalette <- colorPalette[-grep('gray', colorPalette)]
        colorPalette <- colorPalette[-grep('ivory', colorPalette)]
        colorPalette <- colorPalette[-grep('silk', colorPalette)]
        colorPalette <- colorPalette[-grep('bisque', colorPalette)]
        colorPalette <- colorPalette[-grep('lightyellow', colorPalette)]
        colorPalette <- colorPalette[-grep('honeydew', colorPalette)]
        colorPalette <- colorPalette[-grep('palegolden', colorPalette)]
        colorPalette <- colorPalette[-grep('mint', colorPalette)]
        colorPalette <- colorPalette[-grep('lavender', colorPalette)]
        colorPalette <- colorPalette[-grep('lightcyan', colorPalette)]
        colorPalette <- colorPalette[-grep('lemon', colorPalette)]
        colorPalette <- colorPalette[-grep('linen', colorPalette)]
        colorPalette <- colorPalette[-grep('snow', colorPalette)]
        colorPalette <- colorPalette[-grep('almond', colorPalette)]
        colorPalette <- colorPalette[-grep('wheat', colorPalette)]
        colorPalette <- colorPalette[-grep('tan', colorPalette)]
        colorPalette <- colorPalette[-grep('azure', colorPalette)]
        colorPalette <- colorPalette[-grep('papaya', colorPalette)]
        colorPalette <- colorPalette[-grep('lightgolden', colorPalette)]
        
        predFactColors <- colorPalette[sample(1:length(colorPalette), length(unique(predictions)))]
        warnings("If you don't like the randomly chosen color palette, enter 'colors()' in the command line to see the possible colors and rerun this function by supplying valid color names to the 'predFactColors' argument.")
      }
      mapPredictions <- ggplot(belgiumFortified, aes(long, lat, group = group, fill = factor(predicted), text = paste(paste(paste('Postal Code: ', postalCode, sep = ''), ' -- ', sep = ''), Name1, sep = ''))) + xlab('Longitude') + ylab('Latitude') + scale_fill_manual(name = "Predictions", values = predFactColors) + geom_polygon() + ggtitle(paste(plotTitle, ': Predictions', sep = ''))
    }
    
    if(!is.null(plotDir) & !is.null(plotName)){
      if(pdf2) ggsave(paste(plotDir, paste(plotName, '.pdf', sep = ''), sep = ""))
      ggsave(paste(plotDir, paste(plotName, '.png', sep = ''), sep = ""))
    }
    if(randomPick == TRUE & predAsFact == TRUE){
      return(list(belgiumFortified = belgiumFortified, plotTitle = plotTitle, plotDir = plotDir, plotName = plotName, pdf2 = pdf2))
    } else {
      return(mapPredictions)
    }
  }
}
