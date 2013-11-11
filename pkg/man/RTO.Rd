\name{RTO}
\alias{RTO}
\docType{data}
\title{
Regional Tourism Organisation boundaries
}
\description{
A spatial polygons data frame file with the boundaries and names of the Regional Tourism Organisations (RTOs) as in 2012, with some simplifications eg for those in Canterbury.
}
\usage{data(RTO)}
\format{
 Spatial polygons dataframe, in latitude and longitude
 }
\details{
In no way should these boundaries be interpreted as of any meaning other than for statistical convenience in analysing and presenting tourism data.  They do not, for example, exactly match the actual boundaries between RTOs.

This object was created by dissolving area units to match (at least approximately) the RTOs.  The original area units information came from Statistics New Zealand.
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
