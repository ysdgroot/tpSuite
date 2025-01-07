#' Selecting a subset of a single column, selected by its numeric index, of a data.table object, optionally subsetted by some value
#'
#' @param inputDT data.table object containing the data of interest. This is an obligatory argument, without default value.
#' @param rowValue Vector of length 1 that contains the row value of the selected column that should be selected. This is an optional argument, with NULL as default value (in which case all rows are selected).
#' @param colNbr Integer or numeric vector of length 1 that contains the column number of the 'inputDT' object that is selected. This is an obligatory argument, without default value.
#' @param missingsColNbrGone Logical vector of length 1 indicating whether or not one wants to remove the missing values of the selected column. The default value is TRUE and this is an optimal argument only.
#' @return The subset of the 'inputDT' data.table object. If a non-existant value is chosen for the selected column, an empty data.table object is returned.
#' @examples
#' library(data.table)
#' inputDT <- as.data.table(data.frame(x = 10:1, y = LETTERS[1:10]))
#' subsetDT(inputDT, 1, 10)
#' subsetDT(inputDT, 2, 'A')
#' subsetDT(inputDT, 1, 'A')

subsetDT <- function(inputDT, colNbr, rowValue = NULL, missingsColNbrGone = TRUE){
  checkDT(inputDT)
  if(!is.null(rowValue)){
    checkLength(list(rowValue, colNbr), c(1, 1))
  }
  checkLength(list(missingsColNbrGone), 1)
  checkLogicVec(list(missingsColNbrGone))
  if(is.factor(rowValue)) rowValue <- as.character(rowValue)
  if((isCharacterDT(inputDT)[colNbr] | isFactorDT(inputDT)[colNbr]) & !is.character(rowValue)){
    stop("If the 'rowValue' argument is a character or factor value, then the selected column needs be a character or factor variable. This requirement is violated here, so please fix it.")
  } else if(isNumericDT(inputDT)[colNbr] & !is.numeric(rowValue)){
    stop("If the 'rowValue' argument is a numeric value, then the selected column needs be a numeric variable. This requirement is violated here, so please fix it.")
  } else if(isIntegerDT(inputDT)[colNbr] & !is.integer(rowValue)){
    stop("If the 'rowValue' argument is an integer value, then the selected column needs be an integer variable. This requirement is violated here, so please fix it.")
  } else if(isLogicalDT(inputDT)[colNbr] & !is.logical(rowValue)){
    stop("If the 'rowValue' argument is a logical value, then the selected column needs be a logical variable. This requirement is violated here, so please fix it.")
  }
  checkNumOrIntVec(list(colNbr))
  checkRanges(list(colNbr), list(c('>=', 0, '<=', ncol(inputDT))))
  if(is.null(rowValue)){
    if(missingsColNbrGone){
      inputDT <- inputDT[inputDT[, .SD, .SDcols = colNbr]]
      return(inputDT[complete.cases(inputDT[,.SD, .SDcols = colNbr])])
    } else {
      return(inputDT[inputDT[, .SD, .SDcols = colNbr]])
    }
  } else {
    if(missingsColNbrGone){
      inputDT <- inputDT[inputDT[, .I[.SD == rowValue], .SDcols = colNbr]]
      return(inputDT[complete.cases(inputDT[,.SD, .SDcols = colNbr])])
    } else {
      return(inputDT[inputDT[, .I[.SD == (as.character(rowValue))], .SDcols = colNbr]])
    }
  }
}
