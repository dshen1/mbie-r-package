# A function MemoryObjUse() to help with memory management in R.  It lists the memory each object is using.
# Antonia Milkop, Jan 2015.  Via email from James Hogan 

# MemoryObjUse <- function (pos = 1, pat = "") {     
#    
#    dimx <- 
#       function(dd) {
#          if (is.null(dim(dd))) length(dd) else dim(dd) 
#       }
#    
#    lll <- ls(pos = pos, pat = pat) 
#    
#    cat(formatC("mode",  1, 15), 
#        
#        formatC("class", 1, 18), 
#        formatC("name",  1, max(nchar(lll)) + 1), 
#        formatC("Dimensions",1,max(nchar(lll)) ),
#        formatC("Memory Used (Kb)",1,max(nchar(lll)) + 8),
#        
#        "\n----------------------------------------------------------------------------------------\n") 
#    
#    if (length(lll) > 0) 
#    { 
#       for (i in 1:length(lll)) 
#       { 
#          cat(formatC(eval(parse(t = paste("mode(", lll[i], ")"))), 1, 15), 
#              formatC(paste(eval(parse(t = paste("class(", lll[i], ")"))), collapse = " "), 1, 18), 
#              formatC(lll[i],  1, max(nchar(lll)) + 1), " ", 
#              formatC(paste(eval(parse(t = paste("dimx(", lll[i], ")")))), 1, max(nchar(lll)) ),
#              formatC(paste(format(round(object.size(get(lll[i]))/1024), big.mark = ",", justify = c("right"), width = max(nchar(lll))), collapse = " "), 1, max(nchar(lll)) + 8) , "\n"
#          )
#       } 
#    } 
# } 

## the above code have problem to call if compiled in package. so EW changed to following on 20/2/15
MemoryObjUse <- function (pos = 1, pattern = "", ...) {     
   dimx <- 
      function(dd) {
         if (is.null(dim(dd))) length(dd) else dim(dd) 
      }
   
   lll <- ls(pos = pos, pattern = pattern, ...) 
   
   rst <- data.frame(
      mode = sapply(lll,function(x){eval(parse(t = paste("mode(", x, ")")))}),
      class = sapply(lll,function(x){paste(eval(parse(t = paste("class(", x, ")"))), collapse = " ")}),
      name = lll,
      Dimensions = sapply(lll,function(x){paste(eval(parse(t = paste("dimx(", x, ")"))),collapse = ",")}),
      Memory_Used = sapply(lll,function(x){format(round(object.size(x)/1024), big.mark = ",")})
   )
   rownames(rst)[rownames(rst)=="Memory_Used"] <- "Memory Used (Kb)"
   return(rst)
}
