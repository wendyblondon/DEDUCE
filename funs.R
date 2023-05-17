# Functions
numerizer <- function(x){
  as.numeric(unlist(strsplit(x, ",")))
}

sequencer <- function(x){
  seq(1,length(unlist(strsplit(x, ","))))
}

increment_check <- function(x) {
  if (is.character(x)) {
    
    lengthX <- length(unlist(strsplit(x, ",")))
    
    if (lengthX > 1) {
      vecX <- unlist(strsplit(x, ","))
      
      for (i in 1:(lengthX - 1)) {
        if (as.numeric(vecX[i + 1]) > as.numeric(vecX[i]) |
            as.numeric(vecX[1]) > 0 | as.numeric(vecX[lengthX]) < 1) {
          next
        }
        else{
          return(FALSE)
        }
      }
      return(TRUE)
    }
    else{
      return(FALSE)
    }
  }
  else{
    stop("The input must be a character")
  }
}

decimal_check <- function(x) {
  if (is.character(x)) {
    
    lengthX <- length(unlist(strsplit(x, ",")))
    
    if (lengthX > 1) {
      vecX <- unlist(strsplit(x, ","))
      
      for (i in 1:lengthX) {
        if (as.numeric(vecX[i]) > 0 & as.numeric(vecX[i]) < 1) {
          next
        }
        else{
          return(FALSE)
        }
      }
      return(TRUE)
    }
    else{
      return(FALSE)
    }
  }
  else{
    stop("The input must be a character")
  }
}

null_to_na <- function(x){
  ifelse(is.null(x), NA, x)
}

missingvalues_csv <- function(x){
  if(anyNA(x)) {
    return(FALSE)
  }
}

duplicatevalues_csv <- function(x){
  if(sum(duplicated(x)>0)) {
    return(FALSE)
  }
}

columnscheck_csv <- function(x){
  if(ncol(x) !=4 ) {
    return(FALSE)
  }
}
