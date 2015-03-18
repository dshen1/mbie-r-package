\name{YearEnd}
\alias{YearEnd}

\title{
  Convert Time Period to Year End - function
}
\description{
  Function to convert any given time period to a year end, based on a YearEnd setting.
}
\usage{
  YearEnd(TimePeriod, YrEndMthNum)
}
%- maybe also 'usage' for other objects documented here.
\arguments{ 
  \item{TimePeriod}{The date string(vector) with format "YYYY-MM-DD".}
  \item{YrEndMthNum}{The number of Year Ending Month. i.e. 3 for March Year End; 12 for December Year End.}
  
}
\details{
  This function converts any given time period to a year end, based on a YearEnd setting you give it (e.g. 3 for year ending March).  
}
\value{
  }
\references{
  %% ~put references to the literature/web site here ~
}
\author{
  Antonia Milkop <antonia.milkop@mbie.govt.nz>
}
\note{
  %%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~
  
  \seealso{
  }
\examples{
# Example 1 - convert the date 31 March 2014 to a year ending March
YearEnd("2014-03-31",3)
# Example 2 - convert the date 1 April 2014 to a year ending March
YearEnd("2014-04-01",3)
# Example 3 - convert the date 31 April 2014 to a year ending June
YearEnd("2014-04-30",6)
# Example 4 - convert a vector of timePeriods.
YearEnd(c("2014-03-31","2014-04-01"),3)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{}
