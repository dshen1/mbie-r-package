# A function YearEnd(), which converts a given Time Period in TRED data to Year, based on a Year End setting you give it.
# Antonia Milkop, Jan 2015.  Eric Wu developed it

YearEnd <- 
   function(TimePeriod, YearEnd = 12){    
      require(lubridate)
      Year <- year(TimePeriod)
      Year <- ifelse(month(TimePeriod)>YearEnd, Year+1, Year)
      return(Year)
   }
