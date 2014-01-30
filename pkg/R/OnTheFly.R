

#-----------stat_sa, stat_rollapplyr and stat_index12-----------------------------

# the next eighty lines of code create four new "stats" for use
# in ggplot2.  One does rolling average, one seasonal adjustment,
# a third converts to an index based on the average first 12 entries.
# Very handy for adding a smoothed line on the fly.
# 
# WARNING! - to work, the data needs to be sorted by date when it goes in!

require(proto)
require(grid)

# 12 month rolling average stat
# Thanks to https://gist.github.com/holstius/2898533, David Holstius
StatRollApplyR <- proto(ggplot2:::Stat, {   
  required_aes <- c("x", "y")
  default_geom <- function(.) GeomLine
  objname <- "rollapplyr"
  calculate_groups <- function(., data, scales, ...){
    .super$calculate_groups(., data, scales, ...)
  }
  calculate <- function(., data, scales, width=12, FUN, fill=NA, align="right", index12.first=FALSE, ...) {
    if(index12.first==TRUE){data$y <- Index12(data$y)}
    require(zoo)
    filtered <- rollapplyr(data$y, width, FUN=mean, fill=fill, align=align,...)
    result <- data.frame(x=data$x, y=filtered)
    return(result)
  }
}) 
stat_rollapplyr <- StatRollApplyR$new 

# seasonal adjustment stat
StatSA <- proto(ggplot2:::Stat, { 	
  required_aes <- c("x", "y")
  default_geom <- function(.) GeomLine
  objname <- "seasadj"
  calculate_groups <- function(., data, scales, ...){
    .super$calculate_groups(., data, scales, ...)
  }
  calculate <- function(., data, scales, frequency=12, type="multiplicative", index12.first=FALSE, ...) {
    y.ts <- ts(data$y, frequency=frequency)
    if(index12.first==TRUE){y.ts <- Index12(y.ts)}
    if(type=="multiplicative"){
      y.ts <- y.ts / decompose(y.ts, type=type)$seasonal}
    if(type=="additive"){
      y.ts <- y.ts - decompose(y.ts, type=type)$seasonal}
    result <- data.frame(x=data$x, y=as.numeric(y.ts))
    return(result)
  }
}) 
stat_sa <- StatSA$new  

# 12 month reference point index stat
StatIndex12R <- proto(ggplot2:::Stat, { 	
  required_aes <- c("x", "y")
  default_geom <- function(.) GeomLine
  objname <- "index12"
  calculate_groups <- function(., data, scales, ...){
    .super$calculate_groups(., data, scales, ...)
  }
  calculate <- function(., data, scales, ...) {
    filtered <- Index12(data$y, ...)
    result <- data.frame(x=data$x, y=filtered)
    return(result)
  }
}) 
stat_index12 <- StatIndex12R$new 




# index stat using single first point as the index
StatIndex1R <- proto(ggplot2:::Stat, { 	
  required_aes <- c("x", "y")
  default_geom <- function(.) GeomLine
  objname <- "index"
  calculate_groups <- function(., data, scales, ...){
    .super$calculate_groups(., data, scales, ...)
  }
  calculate <- function(., data, scales, ...) {
    filtered <- Index(data$y, ...)
    result <- data.frame(x=data$x, y=filtered)
    return(result)
  }
}) 
stat_index1 <- StatIndex1R$new 
