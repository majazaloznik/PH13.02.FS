

###############################################################################
# 1 FunSkippedGen
###############################################################################


###############################################################################
# FunSkippedGen
# test for skipped generation: is there a missing number in the sequence of generations
###############################################################################

FunSkippedGen <- function(test){
  if(is.na(test[[1]])){return(0)} else {
    if(length(setdiff(seq(min(test, na.rm = TRUE), 
                          max(test, na.rm = TRUE)), 
                      test)) == 0) {
      return(0) } else {
        return(1)}
  }
}


###############################################################################
# FunNumberGens
# number of generations in HH, but test for institutions (first HH is NA)
###############################################################################

FunNumberGens <- function(test){
  if(is.na(test[1])){return(1)} else {
    return(length(unique(test[!is.na(test)])))
  }
}


test <- c(1,2,NA)

FunNumberGens(test)
