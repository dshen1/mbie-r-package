\name{CAGR}
\alias{CAGR}

\title{
Function to calculate compound annual growth rate
}
\description{
A tiny function to calculate compound annual growth rates
}
\usage{
CAGR(ratio, period, digits = 1)
}

\arguments{
  \item{ratio}{
The ratio of the value at the end of the period to its value at the beginning
}
  \item{period}{
Length of time, in whatever units, over which the growth rate is to be calculated
}
  \item{digits = 1}{
Number of decimal places to report. Default is 1.
}
}
\details{
This is simply a code-saving tiny function to calculate a CAGR.  

Both ratio and period can be vectors.  
}

\value{
A vector of the same length as the longest of ratio and period, giving the CAGR/s as a percentage.
}


 \note{
If one of ratio or period is longer than the other the shorter will be recycled as per normal R usage.
}

\author{
Peter Ellis <peter.ellis@mbie.govt.nz>
}

\examples{

# Growth from 100 to 250 over seven years is annual growth of?:
CAGR(2.5, 7, 2)

# Show different CAGRs if it takes different length of time to reach 250%:
plotG(1:7, CAGR(2.5, 1:7),
     xlab = "Number of years to increase to 250 per cent of original level",
     ylab = "Necessary CAGR")
#
}
\keyword{ misc }

