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
        if (vecX[i + 1] > vecX[i]) {
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

decimal_check <- function(x){
  if (is.character(x)) {
    
    lengthX <- length(unlist(strsplit(x, ",")))
    
    if (lengthX > 1) {
      vecX <- unlist(strsplit(x, ","))
      
      for (i in 1:lengthX) {
        if (grepl("[.]", vecX[i]) == FALSE) {
          return(FALSE)
        }
        else{
          next
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
