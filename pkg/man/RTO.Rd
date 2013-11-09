\name{RTO}
\alias{RTO}
\docType{data}
\title{
Regional Tourism Organisation boundaries
}
\description{
A spatial polygons data frame file with the boundaries and names of the Regional Tourism Organisations (RTOs) as in 2012.
}
\usage{data(RTO)}
\format{
 Spatial polygons dataframe, in latitude and longitude
 }
\details{
This object was created by dissolving area units to match (at least approximately) the RTOs
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
\code{\link{IVSvisits}}, \code{\link{IVSspend}}
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
