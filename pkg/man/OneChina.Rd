\name{OneChina}
\alias{OneChina}

\title{
Convert Macau and Hong Kong into China 
}
\description{
Function that takes a factor or character vector and returns a factor with any levels or unique values that correspond to Macau or Hong Kong into 'China, People's Republic of'.
}
\usage{
OneChina(x)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
Vector with class character or factor, with levels or values indicating countries
}

}
\details{
This convenience function is most likely to be invisible to the user but called as part of \code{Country13()}, which has the option of including Hong Kong and Macau in China.

In the tourism industry it is customary to regard Hong Kong and Macau separately from China and for most purposes we group them in "Rest of Asia".  However, for reporting oriented to international relations it can obviously be important to report on the full amount.
}
\value{
A factor of the same length as x but with any levels or unique values that correspond to Hong Kong or Macau converted to 'China, People's Republic of'.
}
\references{
%% ~put references to the literature/web site here ~
}
\author{
Peter Ellis <peter.ellis@mbie.govt.nz>
}
\note{

}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\code{\link{Country13}}
}
\examples{
data(IVStrips)
with(IVStrips, table(Country13(OneChina(COPRDetail)), Country13(COPRDetail)))


# Compare mainland China spend in 2012/13....
with(subset(IVStrips, YEJun==2013 & COPRTop5=="China, People's Republic of"),
     sum(SmoothTotalSpend * FinalWeight * Adjusted_Factor / 10^ 6))

# ... with the spend of the full country:
with(subset(IVStrips, YEJun==2013 & OneChina(COPRDetail)=="China, People's Republic of"),
     sum(SmoothTotalSpend * FinalWeight * Adjusted_Factor / 10^ 6))
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ manip }

