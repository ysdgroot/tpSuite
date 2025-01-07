getMode <- function(vector){
  uniqueValues <- unique(vector)
  uniqueValues[which.max(tabulate(match(vector, uniqueValues)))]
}
