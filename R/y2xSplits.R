#' Convert the y-splits into x-splits
#'
#' @param gamFit gam object
#' @param selectedVar column name for the splits
#' @param min_max_values `numeric(2)` with the minimum and maximum number of the selected variable
#' @param ySplits the splits for the y-direction
#'
#' @returns splits for in the x-direction. 
#' The length will be greater or equal to the length of the ySplits
#' @export
y2xSplits <- function(gamFit, 
                      selectedVar, 
                      min_max_values, 
                      ySplits){
  
  splineData <- splineExtract(gamFit, 
                              selectedVar, 
                              min_max_values)
  
  splineData[, Group := cut(yVar,
                           breaks = c(-Inf, ySplits, Inf))]
  
  # when not in same group, then there is a split the x-direction
  splineData[, EqualGroup := Group == lag(Group)]
  splineData[is.na(EqualGroup), EqualGroup := TRUE]
  
  return(as.numeric(splineData[(!EqualGroup)][["xVar"]]))
}
