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
  
  checkList <- newArgCheck()
  
  lengthObjects <- laply(listObjects, length)
  nElements <- length(lengthObjects)
  comparison <- rep(NA, (nElements*(nElements - 1))/2)
  counter <- 1
  for(iLeft in 1:(nElements - 1)){
    for(iRight in (iLeft + 1):nElements){
      if(!identical(lengthObjects[iLeft], lengthObjects[iRight])){
        if(isObject[iLeft] & isObject[iRight]){
          addError(msg = paste(paste(paste(paste("The length of argument '", namesListObjects[iLeft], sep = ''), "' differs in length from the length of argument '", sep = ''), namesListObjects[iRight], sep = ''), "'.", sep = ''), argcheck = checkList)
        } else if(isObject[iLeft] & !isObject[iRight]){
          addError(msg = paste(paste(paste(paste("The length of argument '", namesListObjects[iLeft], sep = ''), "' differs in length from the length of the element with index ", sep = ''), iRight, sep = ''), '.', sep = ''), argcheck = checkList)
        } else if(!isObject[iLeft] & isObject[iRight]){
          addError(msg = paste(paste(paste(paste("The length of the element with index ", iLeft, sep = ''), " differs in length from the length of argument '", sep = ''), namesListObjects[iRight], sep = ''), "'.", sep = ''), argcheck = checkList)
        } else{
          addError(msg = paste(paste(paste(paste("The length of the element with index ", iLeft, sep = ''), " differs in length from the length of the element with index ", sep = ''), iRight, sep = ''), ".", sep = ''), argcheck = checkList)
        }
      }
    }
  }
  finishArgCheck(checkList)
}

