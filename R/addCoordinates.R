

#' Adding the longitude and latitude coordinate to the input data set when the (Belgian) postal code is known
#'
#' @param inputDT data.table object containing the data of interest. This is an obligatory argument, without default value.
#' @param dirShape Character vector of length 1 indicating directory where all the shape files are stored. This is an obligatory argument, with NULL as default value.
#' @return The longitude and latitude coordinates are added as 2 separate columns to the 'inputDT' argument. Note that no output object is thrown by the 'addCoordinates' function, the 2 columns are directly added to the object that was passed to the 'inputDT' argument.
#' @examples
#'nSamples <- 50000
#'observedValues <- rpois(nSamples, lambda = 1)
#'library(data.table)
#'inputDT <- as.data.table(data.frame(exposure = round(runif(nSamples), digits = 2),
#'observed = observedValues,
#'postalCode = sample(c('1000', '3000', '5000'), nSamples, replace = TRUE)))
#'addCoordinates(inputDT)
#'#addCoordinates(inputDT, '/home/robin/bla/')

addCoordinates <- function(inputDT, dirShape = NULL){
  
  checkDT(inputDT, 'postalCode')
  if(!isNumericDT(inputDT, 'postalCode')) asNumericDT(inputDT, 'postalCode')
  checkCharVec(list(dirShape))
  
  checkLength(list(dirShape), 1)
  lastCharDir <- substr(dirShape, start = nchar(dirShape), stop = nchar(dirShape))
  if(lastCharDir == '\\' | lastCharDir == '/'){
    dirShape <- substr(dirShape, start = 1, stop = nchar(dirShape) - 1)
  }
  
  if(sum(names(inputDT) == 'LAT')) inputDT[, LAT := NULL]
  if(sum(names(inputDT) == 'LONG')) inputDT[, LONG := NULL]
  if(sum(names(inputDT) == 'lat')) inputDT[, lat := NULL]
  if(sum(names(inputDT) == 'long')) inputDT[, long := NULL]
  
  didItLoad <- suppressWarnings(try(postalCode <- as.data.table(read.csv(paste(dirShape, "zipcodes.csv", sep = "\\"))[,c("CODPOSS","LAT", "LONG")]), silent = TRUE))
  if('try-error' %in% class(didItLoad)) stop("Please set a valid directory to the argument 'dirShape'.")
  
  postalCode <- as.data.table(read.csv(paste(dirShape, "zipcodes.csv", sep = "\\"))[,c("CODPOSS","LAT", "LONG")])
  setnames(postalCode, c('CODPOSS', 'LONG', 'LAT'), c('postalCode', 'longitude', 'latitude'))
  setkey(inputDT, 'postalCode')
  setkey(postalCode, 'postalCode')
  return(merge(inputDT, postalCode, all.x = TRUE))
}