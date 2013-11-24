\name{CountryGroup}
\alias{CountryGroup}
\alias{Country13}

\title{
Convert "detailed" countries into a grouping
}
\description{
Function that takes a factor or character vector and returns a factor with approximately 13 levels (depending on the "type" argument), matching either the country groupings in the IVS as used from July 2013, or Tourism New Zealand's country groupings used for their reporting.
}
\usage{
CountryGroup(x, shorten=TRUE, type="IVSweights", OneChina_first=FALSE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
Vector with class character or factor, with levels or values indicating countries
}
 \item{shorten}{
Logical - should the long names for China, USA and UK be abbreviated?
}
 \item{type}{
Character - must be either "IVSweights" or "TNZ2013" - should the grouping be into the thirteen country groupings used for IVSweights, or the groupings used by Tourism New Zealand for their reporting in 2013?
}
 \item{OneChina_first}{
Logical - Should Macau and Hong Kong, and their variant names, be merged into 'China, People's Republic of' or (the default) be grouped under 'Rest of Asia'?
} 

}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
A factor of the same length as x but with 13 levels.
}
\references{
%% ~put references to the literature/web site here ~
}
\author{
Peter Ellis <peter.ellis@mbie.govt.nz>
}
\note{
The country groupings provided by "TNZ2013" are not exactly as might be expected.  Specifically:
\itemize{
\item "Other Pen. South East Asia" consists only of the three countries Malaysia, Singapore and Thailand (it excludes, for example, Brunei).
\item "Latin America" consists only of the four countries Argentina, Brazil, Chile, Mexico (it excludes Columbia, Peru, and many others)
}

\code{Country13()} is an old version of this function and its use is deprecated.  \code{CountryGroup()} may eventually be expanded to give convenience groupings with a different number of levels from 13 and the name has been changed to reflect this.  For now (as at version 0.6.1 of the mbie package) Country13() will still work but it is deprecated
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\code{\link{IVStrips}}, \code{\link{OneChina}}
}
\examples{

# Total sample size over 16.5 years of the old IVS
data(IVStrips)
table(CountryGroup(IVStrips$COPRDetail))
table(CountryGroup(IVStrips$COPRDetail, shorten=FALSE))
table(CountryGroup(IVStrips$COPRDetail, OneChina=TRUE))
table(CountryGroup(IVStrips$COPRDetail, type="TNZ2013", shorten=TRUE))
}


\keyword{ manip }

