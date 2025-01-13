#' Categorization of continuous according to some splitting points.
#'
#' @param inputDT data.table object containing the data of interest. 
#' This is an obligatory argument, without default value.
#' @param splits Named list of numeric vectors
#' The names of this object need to correspond to column names of the 'inputDT' argument. 
#' This is an obligatory argument, without default value.
#' 
#' @return The variables whose names are found in the 'splits' argument as names of the object that is passed to this argument, 
#' are splitted by means of the splitting points that are supplied by the same 'split' argument. 
#' The resulting variable is added to the object that is passed to the 'inputDT' argument, 
#' and the respective column name corresponds to the variable name augmented with the suffix 'Grouped', 
#' without any space or character in between. 
#' Note that the grouping is always performed in the following manner: 
#' if the 'split' argument supplies the splitting points a, b, and c, then the continuous variable is split up in the following intervals ]-infinity, a[, [a, b[ and [b, +infinity[.
#' @export
transform2BinnedVar <- function(inputDT, 
                                splits){
  # checks

  if (!is.list(splits)) {
    stop("The 'split' argument is a list of numeric vectors.")
  }
  
  varNames <- names(splits)
  checkDT(inputDT, varNames)
  
  # end checks

  # sort the splits given
  sorted_splits <- lapply(splits, sort)
  breaks <- lapply(sorted_splits, function(xx) c(-Inf, xx, Inf))
  
  for (column_name in names(breaks)) {
    
    breaks_col <- breaks[[column_name]]
    if (all(!is.na(breaks_col))) {
      new_name <- sprintf("%sGrouped", 
                          column_name)
      inputDT[, (new_name) := cut(get(column_name),
                                  breaks_col, 
                                  ordered_result = TRUE)]
    } else {
      warning(sprintf("No splits were provided by the 'splits' argument for the variable %s", 
                      varNames[iVar]))
    }
  }
}
