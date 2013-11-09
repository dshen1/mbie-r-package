

LongToStatPlanet <- function(long, file){
  # the function will start here.  Input will be long, a data frame
  # with columns with the names category, indicator, time, region and value
  
  if(sum(!c("category", "indicator", "time", "region", "value") %in% names(long)) > 0  ){
    stop("'long' must have columns named
         'category', 'indicator', 'time', 'region' and 'value'")
  }
  
  regions <- as.character(unique(long$region))
  
  # create a wide version in similar structure to final output
  wide <- dcast(long, category + indicator + time ~ region, sum, 
                value.var="value")
  
  # make indicator a character, not factor, to avoid complications
  wide$indicator <- as.character(wide$indicator)
  
  # create skeleton of output data frame
  statplanet <- cbind(data.frame(CATEGORY="", TIME="", INDICATOR="",  SOURCE="",
                                 DESCRIPTION="",  UNIT="",	MAP="",	GRAPH="",
                                 FILE="",	OPTIONS="",	TYPE=""),
                      matrix("", ncol=length(regions)))
  
  # make all the columns of statplanet character, not numeric or factor
  for(i in 1: ncol(statplanet)){
    statplanet[ , i] <- as.character(statplanet[ , i])
  }
  
  # give the correct region names to columns 12 and onwards as needed by StatPlanet
  names(statplanet)[-(1:11)] <- regions
  
  # categories is a vector of the names of all the categories of indicators
  categories <- as.character(unique(long$category))
  
  # row-number will count through the rows as we make them
  row_number <- 1
  
  # i will loop through the values of categories
  for(i in 1: length(categories)){
  
    # new set of categories
    statplanet[row_number, "CATEGORY"] <- categories[i]

    # next command seems counter to the statplanet spec but is needed to 
    # make it work
    row_number <- row_number + 1
    
    # within this category we need to know the unique times and indicators
    times <- unique(long$time)
    times <- times[order(-times)]
    indicators <- as.character(unique(wide$indicator))
    
    
    # now within this category we loop through all the times
    for(j in 1: length(times)){
      statplanet[row_number, "TIME"] <- times[j]
      row_number <- row_number + 1
      
      # and within this time we grab a rectangle of all the indicators
      tmp <- with(wide, wide[time==times[j] & category == categories[i], 
                             c("indicator", regions)])
      
      # and put it into our statplanet rectangle:
      statplanet[row_number : (row_number + nrow(tmp) - 1), c("INDICATOR", regions)] <- tmp
      
      row_number <- row_number + nrow(tmp)
    }
  
  }
  # replace all the NAs with blanks
  statplanet[is.na(statplanet)] <- ""
  
  # write to the file location specified earlier
  write.csv(statplanet[1:nrow(statplanet), ], file=file, row.names=FALSE)
  
  #TODO - sub categories, sources
}
