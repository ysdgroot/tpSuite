#TODO this can be update using a MessageHandler
newArgCheck <- function() {
  argcheck <- new.env()
  assign("n_warn", 0, envir = argcheck)
  assign("warn_msg", NULL, envir = argcheck)
  assign("n_error", 0, envir = argcheck)
  assign("error_msg", NULL, envir = argcheck)
  assign("n_message", 0, envir = argcheck)
  assign("message_msg", NULL, envir = argcheck)
  class(argcheck) <- c("ArgCheck", "environment")
  return(argcheck)
}

#TODO this can be update using a MessageHandler
addError <- function(msg, argcheck){
  if(!"ArgCheck" %in% class(argcheck)) stop("'argcheck' must be an object of class 'ArgCheck'")
  assign("n_error", get("n_error", envir = argcheck) + 1, envir = argcheck)
  assign("error_msg", c(get("error_msg", envir = argcheck), msg), envir = argcheck)
}
#TODO this can be update using a MessageHandler
finishArgCheck <- function(argcheck, justWarnings = FALSE){
  
  fn_call <- sys.call(-1)
  fn_call <- utils::capture.output(fn_call)
  if(!"ArgCheck" %in% class(argcheck)) stop("'argcheck' must be an object of class 'ArgCheck'")
  argcheck <- mget(ls(envir = argcheck), envir = argcheck)
  if(argcheck$n_warn > 0)
    warning(paste0(c("", fn_call, paste0(1:argcheck$n_warn, ": ", argcheck$warn_msg)), collapse = "\n"), call. = FALSE)
  if(argcheck$n_message > 0)
    message(paste0(c("", fn_call, paste0(1:argcheck$n_message, ": ", argcheck$message_msg)), collapse = "\n"))
  if(argcheck$n_error > 0){
    if(justWarnings == FALSE){
      stop(paste0(c("", fn_call, paste0(1:argcheck$n_error, ": ", argcheck$error_msg)), collapse = "\n"), call. = FALSE)
    } else {
      warning(paste0(c("", fn_call, paste0(1:argcheck$n_error, ": ", argcheck$error_msg)), collapse = "\n"), call. = FALSE)
    }
  }
}



# -------------------------------------------------------------------------


checkEqualLength <- function(listObjects, ...){
  
  namesListObjects <- deparse(substitute(listObjects))
  if(str_count(namesListObjects, "list[(]") > 1){
    stop('All the objects to be checked need to be put in a list. A list of lists is not permitted by this function.')
  }
  if(nargs() > 1){
    stop("Just one separate argument needs to be provided for.")
  }
  if(length(listObjects) == 1){
    stop("The names of at least 2 different objects should be stored in the 'listObjects' argument.")
  }
  
  namesListObjects <- unlist(strsplit(namesListObjects, '[,]'))
  namesListObjects <- gsub("\\s", "", namesListObjects)
  namesListObjects <- str_replace_all(namesListObjects, fixed("list("), "")
  namesListObjects <- str_replace_all(namesListObjects, fixed(")"), "")
  isObject <- aaply(namesListObjects, 1, function(xx) exists(xx, envir = parent.frame()))
  
  #checkList <- newArgCheck()
  checkList <- MessageHandler$new()
  
  lengthObjects <- laply(listObjects, length)
  nElements <- length(lengthObjects)
  comparison <- rep(NA, (nElements*(nElements - 1))/2)
  counter <- 1
  for(iLeft in 1:(nElements - 1)){
    for(iRight in (iLeft + 1):nElements){
      if(!identical(lengthObjects[iLeft], lengthObjects[iRight])){
        if(isObject[iLeft] & isObject[iRight]){
          message <- sprintf("The length of argument '%s' differs in length from the length of argument '%s'.", 
                             namesListObjects[iLeft], 
                             namesListObjects[iRight])
        } else if(isObject[iLeft] & !isObject[iRight]){
          message <- sprintf("The length of argument '%s' differs in length from the length of the element with index %d", 
                             namesListObjects[iLeft], 
                             iRight)
         } else if(!isObject[iLeft] & isObject[iRight]){
           message <- sprintf("The length of the element with index %d differs in length from the length of argument '%s'.", 
                              iLeft, 
                              namesListObjects[iRight])
         } else{
          message <- sprintf("The length of the element with index %d differs in length from the length of the element with index %d",
                             iLeft, 
                             iRight)
        }
        checkList$add_error(message)
      }
    }
  }
  #finishArgCheck(checkList)
  checkList$finish()
}

#' Checking if all elements of a list are all numeric or integer vectors with just whole numbers
#'
#' @param listNum A list of the vectors of which one wishes to check if their data type is numeric
#' @param namesListElements Character vector containing the names of the variables of which the data type is checked. Optional parameter, with as default value NULL. 
#' This argument should be used when the variable of which the data type is checked is not an object that was provided as an argument to the function, 
#' or when the list elements of the first argument do not have a name attached to it.
#' 
#' @return No value is returned if all vectors have the numeric or integer data type, containing just whole numbers. 
#' If not, an error message is thrown for each element of the list that does not pertain to the numeric data type.
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
  
  checkList <- MessageHandler$new()
  
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
      if(!all(listNum[[iList]] == round(listNum[[iList]])) & 
         !is.null(listNum[[iList]])){
        if(isObject[iList]){
          message <- sprintf("The argument '%s' does NOT consist of whole numbers.", 
                             argNames[iList])
            
        } else if(is.null(namesListElements)){
            message <- sprintf('The element with index %d does NOT consist of whole numbers.', 
                               iList)
        } else {
            message <- sprintf("The argument '%s' does NOT consist of whole numbers.", 
                               namesListElements[iList])
        }
        checkList$add_error(message)
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
    checkList$add_error('The "listNum" argument should be of a list.')
  }
  #finishArgCheck(checkList)
  checkList$finish()
}


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
