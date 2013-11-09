

LongToStatPlanet <- function(long, file){
  # the function will start here.  Input will be long, a data frame
  # with columns with the names category, indicator, time, region and value
  
  if(sum(!c("category", "subcategory", "indicator", "time", "region", "value") %in% names(long)) > 0  ){
    stop("'long' must have columns named
         'category', 'subcategory', 'indicator', 'time', 'region' and 'value'")
  }
  
  regions <- as.character(unique(long$region))
  
  # create a wide version in similar structure to final output
  wide <- dcast(long, category + subcategory + indicator + time ~ region, sum, 
                value.var="value")
  
  # make indicator a character, not factor, to avoid complications
  wide$indicator <- as.character(wide$indicator)
  wide$category <- as.character(wide$category)
  wide$subcategory <- as.character(wide$subcategory)
  
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
  # i <- 1 # for debugging
  for(i in 1: length(categories)){
  
    # new set of categories
    statplanet[row_number, "CATEGORY"] <- categories[i]

    
    
    # within this category we need to know the unique subcategories
    subcats <- unique(long$subcategory[long$category==categories[i]])
    
    for(k in 1: length(subcats)){
      # k <- i # for debugging
      
      # not clear from the spec exactly where we need empty lines but it appears to be before each subcategory
      row_number <- row_number + 1
      statplanet[row_number, "CATEGORY"] <- paste0(">", subcats[k])
      
      long2 <- long[long$subcategory==subcats[k] & long$category==categories[i], ]
      wide2 <- wide[wide$subcategory==subcats[k] & wide$category==categories[i], ]
      
      # within this subcategory we need to know the unique times and indicators
      times <- unique(long2$time)
      times <- times[order(-times)]
      indicators <- as.character(unique(wide2$indicator))
      
      
      # now within this category we loop through all the times
      for(j in 1: length(times)){
        statplanet[row_number, "TIME"] <- times[j]
        row_number <- row_number + 1
        
        # and within this time we grab a rectangle of all the indicators
        tmp <- with(wide2, wide2[time==times[j], c("indicator", regions)])
        
        # and put it into our statplanet rectangle:
        statplanet[row_number : (row_number + nrow(tmp) - 1), c("INDICATOR", regions)] <- tmp
        
        row_number <- row_number + nrow(tmp)
      }
    
    }
    
  }
    # replace all the NAs with blanks
    statplanet[is.na(statplanet)] <- ""
    row.names(statplanet) <- NULL
  
  # write to the file location specified earlier
  # Note - TODO - a bug here, must be something to do with file encoding.  The data file produced
  # by the following will crash StatPlanet, but if you just open it in Excel and save as csv, without
  # changing anything, it works fine.
  write.csv(statplanet, file=file, row.names=FALSE)
  
  #TODO - sources, stories, more flexibility in the use of categories, subcategories and subsubcategories
}
