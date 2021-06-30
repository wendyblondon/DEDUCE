# Functions
numerizer <- function(x){
  as.numeric(unlist(strsplit(x, ",")))
}

sequencer <- function(x){
  seq(1,length(unlist(strsplit(x, ","))))
}

incrementCheck <- function(x) {
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

decimalCheck <- function(x){
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

nullToNA <- function(x){
  ifelse(is.null(x), NA, x)
}

designInputs <- function(x){
  
  trues <- which(x==TRUE)
  
  for (i in trues) {
    if (i == 1) {
      x[i] <-"3+3"
    }
    else if (i == 2) {
      x[i] <- "TARGET-CRM"
    }
    
    else{
      x[i] <- "CRM"
    }
  }
  
  x <- x[trues]
  return(x)
}
