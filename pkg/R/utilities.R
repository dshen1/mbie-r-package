
despace <- function(x){
  return(gsub(" ", "_", x, fixed=TRUE))
}

FormatDollars <- function(x, endmark="", ...){
  x <- paste0("$", format(round(x, ...), big.mark=","), endmark) 
  x <-gsub(" ", "", x)
  return(x)
}

# wrap <- function(s, n=10){
#   # function inserts line breaks every 10 (or n) characters
#   # adapted from http://stackoverflow.com/questions/2351744/insert-line-breaks-in-long-string-word-wrap
#   
#   tmp <- gsub(paste0('(.{1,', n, '})(\\s|$)'), '\\1\n', s)
#   
#   # strip off the last 2 characters
#   nc <- nchar(tmp)
#   tmp <- substring(tmp, 1, nc-1)
#   return(tmp)
# }
# 

wrap <- function(s, n=10, sep="\n"){
   if(!sep %in% c("\n", "<br>")){
      stop("sep must be \\n or <br>")
   }
   if(sep == "\n"){
      tmp <- gsub(paste0("(.{1,", n, "})(\\s|$)"), "\\1\n", s)  
      nc <- nchar(tmp)
      tmp <- substring(tmp, 1, nc - 1)
      
   } else {
      tmp <- gsub(paste0("(.{1,", n, "})(\\s|$)"), "\\1<br>", s)  
   }
   return(tmp)
}

#---------------Index and Index12---------------


Index12 <- function(x, ref=mean(x[1:12]), stripNAs = FALSE){
  # Function that takes a vector or time series (or any object actually) 
  # and returns it as an index based on 100.  Defaults to using the 
  # mean of the first 12 elements of the vector as reference value,  
  # but this can be overridden.
    if(stripNAs){
    x <- x[!is.na(x)]
  }
  if(length(ref)>1) stop("Reference value must be a single number")
  x*100/ref
}


Index <- function (x, ref = x[1], stripNAs = FALSE) 
{
  if(stripNAs){
    x <- x[!is.na(x)]
  }
  if (length(ref) > 1) 
    stop("Reference value must be a single number")
  x * 100/ref
}