\name{YearEnd}
\alias{TimePeriod}

\title{
  Convert Time Period to Year End - function
}
\description{
  Function to convert any given time period to a year end, based on a YearEnd setting.
}
\usage{
  YearEnd("YYYY-MM-DD", x)
}
%- maybe also 'usage' for other objects documented here.
\arguments{ x   Numeric YearEnd setting , i.e. 3 for March Year End; 12 for December Year End
  \item{x}{}
  
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
    \code{\link{Memory}}
  }
\examples{
require(lubridate)
# Example 1 - convert the date 31 March 2014 to a year ending March
YearEnd("2014-03-31",3)
# Example 1 - convert the date 31 April 2014 to a year ending March
YearEnd("2014-04-31",3)
# Example 1 - convert the date 31 April 2014 to a year ending June
YearEnd("2014-04-31", 6)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{}
