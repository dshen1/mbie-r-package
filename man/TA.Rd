\name{TA}
\alias{TA}
\docType{data}
\title{
Territorial Authority boundaries
}
\description{
A spatial polygons data frame file with the boundaries and names of New Zealand's Territorial Authorities as in 2012.
}
\usage{data(TA)}
\format{
 Spatial polygons dataframe, in latitude and longitude
 }
\details{
This object was created by importing a shapefile with \code{readOGR()}, downloaded from the Statistics New Zealand website.
}
\source{
New Zealand Ministry of Business, Innovation and Employment
}
\references{
www.mbie.govt.nz
}

\note{
Mapping seems to work better in an R console (eg in conjunction with Notepad++) than in RStudio.
}

\seealso{
\code{\link{TA_TRED}}, \code{\link{Region}}
}

\examples{
require(sp)
data(RTO)
data(TA)
par(fg=NA)
plot(RTO, col=tourism.cols("Alternating"))
par(fg="black")
plot(TA, add=TRUE)
axis(1)
axis(2)

}
\keyword{datasets}
