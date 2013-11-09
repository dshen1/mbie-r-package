\name{Region}
\alias{Region}
\docType{data}
\title{
Regional Council boundaries
}
\description{
A spatial polygons data frame file with the boundaries and names of New Zealand's Regional Councils as in 2012.
}
\usage{data(Region)}
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
\code{\link{TA}}
}

\examples{
require(sp)
data(Region)
data(TA)
par(fg=NA)
plot(Region, col=tourism.cols("Alternating"))
par(fg="black")
plot(TA, add=TRUE)
centers <- coordinates(Region)
text(centers[, 1], centers[,2], labels=row.names(centers), cex=.5, col="grey50")
axis(1)
axis(2)

}
\keyword{datasets}
