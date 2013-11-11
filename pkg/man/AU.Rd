\name{AU}
\alias{AU}
\docType{data}
\title{
Area Unit boundaries
}
\description{
A spatial polygons data frame file with the boundaries and names of New Zealand's Area Units as in 2012.
}
\usage{data(Region)}
\format{
 Spatial polygons dataframe, in latitude and longitude
 }
\details{
This object was created by importing a shapefile, downloaded from the Statistics New Zealand website, with \code{readOGR()} and using \code{spTransform} to project it in latitude and longitude (rather than NZTM coordinates).  The boundaries have not been adjusted in any way.
}
\source{
Statistics New Zealand
}
\references{
\code{\link{http://www.stats.govt.nz/browse_for_stats/people_and_communities/Geographic-areas/digital-boundary-files.aspx}}
}

\note{
Mapping seems to work better in an R console (eg in conjunction with Notepad++) than in RStudio.
}

\seealso{
\code{\link{Region}}, \code{\link{TA}}
}

\examples{
require(sp)
data(AU)
plot(AU)
axis(1)
axis(2)

}
\keyword{datasets}
