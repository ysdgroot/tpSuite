#TODO: document
#TODO function is not even used...
addLevelDT <- function(inputDT, 
                       catVar, 
                       newLevel){
  
  checkCharVec(list(catVar, newLevel))
  checkLength(list(catVar, newLevel), 1)
  checkDT(inputDT, catVar)
  
  indCatVar <- which(names(inputDT) == catVar)
  selFact <- inputDT[, .SD, .SDcols = indCatVar]
  levels(selFact[[1]]) <- unique(c(levels(selFact[[1]]), newLevel))
  set(inputDT, NULL, as.integer(indCatVar), selFact)
  
}

