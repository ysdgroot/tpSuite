isPresentCharVec <- function(inputVector, value2BeChecked){
  if(!is.character(inputVector) & !is.factor(inputVector)){
    stop("The 'inputVector' argument should be a factor or a character vector.")
  }
  if(!is.character(value2BeChecked) & !is.factor(value2BeChecked)){
    stop("The 'value2BeChecked' argument should be a factor or a character vector.")
  }
  if(length(which(inputVector == value2BeChecked)) > 0){
    return(TRUE)
  } else {
    return(FALSE)
  }
}


