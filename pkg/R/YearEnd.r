# A function YearEnd(), which converts a given Time Period in TRED data to Year, based on a Year End setting you give it.
# Antonia Milkop, Jan 2015.  Eric Wu developed it

YearEnd <- function(TimePeriod, YearEnd = 12){    
          require(lubridate)
          Year <- year(TimePeriod)
          Year <- ifelse(month(TimePeriod)>YearEnd, Year+1, Year)
          return(Year)
      }

require(lubridate)
# Example 1 - convert the date 31 March 2014 to a year ending March
YearEnd("2014-03-31", 3)
# Example 1 - convert the date 1 April 2014 to a year ending March
YearEnd("2014-04-01", 3)