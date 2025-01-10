#' Select the best splits based on the explained variance
#'
#' @param explVar list with all the explained variances
#' @param nGroups list with all the explained nGroups
#' @param splittingPoints list with all the splitting points. 
#' @param minIncr single numeric value. 
#' Indicating the minimum increase of the explained Variance
#' @param minIncrRel single numeric value.
#' Indicating the minimum relative increase of the explained variance
#'
#' @returns list with the best solutions of length 3 with names: 
#' `ExplainedVariance`: best explained variance
#' `nGroups`: Number of groups with had the best explained variance
#' `SplittingPoints`: poisitions of the best splits
#' 
#' @export
getBestnGroups <- function(explVar, 
                           nGroups, 
                           splittingPoints, 
                           minIncr = 0.05, 
                           minIncrRel = 1.05){
  
  if(length(explVar) != length(nGroups) |
     length(nGroups) != length(splittingPoints)){
    stop("All lengths of the variables needs to be equal")
  }
  
  if(any(is.na(explVar))){
    stop("NA values in 'explVar' are found, it should be a number")
  }
  
  n_groups <- length(nGroups)
  
  best_variance <- explVar[[1]]
  best_groups <- nGroups[[1]]
  best_splittingPoint <- splittingPoints[[1]]
  
  if(length(explVar) > 1){
    #TODO: check if this is correct
    # minimal increase: 
    selGroup <- which(explVar[2:n_groups] - explVar[1:(n_groups - 1)] < minIncr)
    # minimal relative increase
    selGroupRel <- which(explVar[2:n_groups] < minIncrRel * explVar[1:(n_groups - 1)])
    
    if (length(selGroup) > 1) {
      selGroup <- max(selGroup)
    } 
    if (length(selGroupRel) > 1) {
      selGroupRel <- max(selGroupRel)
    } 
    if (length(selGroup) == 0) {
      selGroup <- length(nGroups)
    }
    if (length(selGroupRel) == 0) {
      selGroupRel <- length(nGroups)
    } 
    
    best_position <- min(selGroup, selGroupRel)
    
    best_splittingPoint <- splittingPoints[[best_position]]
    best_variance <- explVar[[best_position]]
    best_groups <- nGroups[[best_position]]
  } 
  
  return(list("ExplainedVariance" = best_variance, 
              "nGroups" = best_groups, 
              "SplittingPoints" = best_splittingPoint))
}

