#TODO: document

checkPlotDirAndName <- function(plotDir = NULL, plotName = NULL){
  if((is.null(plotDir) & !is.null(plotName)) | (!is.null(plotDir) & is.null(plotName))) stop('If one want to plot the figure automatically, then one needs to provide a value for both the "plotDir" argument and the "plotName" argument.')
  if(!is.null(plotName)){
    checkCharVec(list(plotName))
    checkLength(list(plotName), 1)
    if(length(unlist(strsplit(plotName, '[.]'))) > 1) stop('The "plotName" argument should not contain the extension.')
  }
  if(!is.null(plotDir)){
    checkCharVec(list(plotDir))
    checkLength(list(plotDir), 1)
    if(str_sub(plotDir, start= -1) != '/') plotDir <- paste(plotDir, '/', sep = "")
  }
}