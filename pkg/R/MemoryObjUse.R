# A function MemoryObjUse() to help with memory management in R.  It lists the memory each object is using.
# Antonia Milkop, Jan 2015.  Via email from James Hogan 
MemoryObjUse <- function (pos = 1, pat = "") {     
  
    dimx <- function(dd) if (is.null(dim(dd))) 
            length(dd) 
            else dim(dd) 
    
    lll <- ls(pos = pos, pat = pat) 
    
    cat(formatC("mode",  1, 15), 
    
    formatC("class", 1, 18), 
    formatC("name",  1, max(nchar(lll)) + 1), 
    formatC("Dimensions",max(nchar(lll)) + 10),
    formatC("Memory Used (Kb)",max(nchar(lll)) + 35),
    
        "\n-----------------------------------------------------------------\n") 
    
    if (length(lll) > 0) 
    { 
    for (i in 1:length(lll)) 
    { 
      cat(formatC(eval(parse(t = paste("mode(", lll[i], ")"))), 1, 15), 
          formatC(paste(eval(parse(t = paste("class(", lll[i], ")"))), collapse = " "), 1, 18), 
          formatC(lll[i],  1, max(nchar(lll)) + 1), " ", 
          formatC(paste(eval(parse(t = paste("dimx(", lll[i], ")")))), 1, 2),
          formatC(paste(format(round(object.size(get(lll[i]))/1024), big.mark = ",", justify = c("right")), collapse = " "), 1, 15) , "\n"
          )
    } 
    } 
    } 

MemoryObjUse()
  