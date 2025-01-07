#' Checking if all elements of a list are all numeric or integer vectors with just whole numbers
#'
#' @param listNum A list of the vectors of which one wishes to check if their data type is numeric
#' @param namesListElements Character vector containing the names of the variables of which the data type is checked. Optional parameter, with as default value NULL. This argument should be used when the variable of which the data type is checked is not an object that was provided as an argument to the function, or when the list elements of the first argument do not have a name attached to it.
#' @return No value is returned if all vectors have the numeric or integer data type, containing just whole numbers. If not, an error message is thrown for each element of the list that does not pertain to the numeric data type.
#' @examples
#' arg1 <- 2
#' checkWholeNumb(list(arg1))
#' checkWholeNumb(list(2))
#'
#' arg2 <- 0.8
#' \donttest{checkWholeNumb(list(arg2))}
#' \donttest{checkWholeNumb(list(1, arg2))}

checkWholeNumb <- function(listNum, namesListElements = NULL){
  
  if(!is.list(listNum)) stop("The argument 'listNum' needs to be a list.")
  
  for(iList in 1:length(listNum)){
    if(is.list(listNum[[iList]])){
      stop(paste(paste('The data type of element with index ', iList, sep = ''), ' is NOT a vector. A list of lists is not permitted by this function.', sep = ''))
    }
  }
  
  checkList <- newArgCheck()
  argNames <- deparse(substitute(listNum))
  argNames <- unlist(strsplit(argNames, '[,]'))
  argNames <- gsub("\\s", "", argNames)
  argNames <- str_replace_all(argNames, fixed("list("), "")
  argNames <- str_replace_all(argNames, fixed(")"), "")
  isObject <- aaply(argNames, 1, function(xx) exists(xx, envir = parent.frame()))
  
  if(!is.null(namesListElements)){
    if(!is.character(namesListElements)) stop("The argument 'namesListElements' should be a character vector or string.")
    if(length(listNum) != length(namesListElements)) stop("The argument 'listNum' should have the same length as argument 'namesListElements'.")
  }
  
  if(is.list(listNum)){
    for(iList in 1:length(listNum)){
      checkNumOrIntVec(list(listNum[[iList]]))
      if(!all(listNum[[iList]] == round(listNum[[iList]])) & !is.null(listNum[[iList]])){
        if(isObject[iList]){
          addError(msg = paste(paste("The argument '", argNames[iList], sep = ''), "' does NOT consist of whole numbers.", sep = ''), argcheck = checkList)
        } else {
          if(is.null(namesListElements)){
            addError(msg = paste(paste('The element with index ', iList, sep = ''), ' does NOT consist of whole numbers.', sep = ''), argcheck = checkList)
          } else {
            addError(msg = paste(paste("The argument '", namesListElements[iList], sep = ''), "' does NOT consist of whole numbers.", sep = ''), argcheck = checkList)
          }
        }
      }
    }
  } else if(is.vector(listNum) & length(listNum) == 1){
    checkNumOrIntVec(list(listNum))
    if(!all(listNum == round(listNum)) & !is.null(listNum)){
      if(isObject){
        addError(msg = paste(paste("The argument '", argNames, sep = ''), "' does NOT consist of whole numbers.", sep = ''), argcheck = checkList)
      } else {
        if(is.null(namesListElements)){
          addError(msg = paste(paste('The element with index ', '1', sep = ''), ' does NOT consist of whole numbers.', sep = ''), argcheck = checkList)
        } else {
          addError(msg = paste(paste("The argument '", namesListElements, sep = ''), "' does NOT consist of whole numbers.", sep = ''), argcheck = checkList)
        }
      }
    }
  } else {
    addError(msg = 'The "listNum" argument should be of a list.', argcheck = checkList)
  }
  finishArgCheck(checkList)
}
