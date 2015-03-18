# A function YearEnd(), which converts a given Time Period in TRED data to Year, based on a Year End setting you give it.
# Antonia Milkop, Jan 2015.  Eric Wu developed it

YearEnd <- 
   function(TimePeriod, YrEndMthNum = 12){    
      Year <- lubridate::year(TimePeriod)
      Year <- ifelse(lubridate::month(TimePeriod)>YrEndMthNum, Year+1, Year)
      return(Year)
   }
