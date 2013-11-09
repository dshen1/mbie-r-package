\name{TA_TRED}
\alias{TA_TRED}
\docType{data}
\title{
Modified Territorial Authority boundaries
}
\description{
A spatial polygons data frame file with the boundaries and names of New Zealand's Territorial Authorities as in 2012, but slightly modified from the official boundaries.  Specifically, Auckland is split into six; southland into two; Queenstown/Wanaka into two; and Northland into three.  
}
\usage{data(TA_TRED)}
\format{
 Spatial polygons dataframe, in latitude and longitude
 }
\details{
This object was created by dissolving area units.
}
\source{
New Zealand Ministry of Business, Innovation and Employment
}
\references{
www.mbie.govt.nz
}

\seealso{
\code{\link{TA}}, \code{\link{Region}}
}
\note{
Mapping seems to work better in an R console (eg in conjunction with Notepad++) than in RStudio.
}

\examples{
require(sp)
data(TA_TRED)
data(TA)
par(fg=NA)
plot(TA, col=tourism.cols("Alternating"))
par(fg="black")
plot(TA_TRED, add=TRUE)
centers <- coordinates(TA_TRED)
text(centers[, 1], centers[,2], labels=row.names(centers), cex=.5, col="grey50")
axis(1)
axis(2)

}
\keyword{datasets}
