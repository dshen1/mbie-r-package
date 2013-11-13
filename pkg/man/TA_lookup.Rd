\name{TA_lookup}
\alias{TA_lookup}
\docType{data}
\title{
TA_lookup
}
\description{
Reference table showing which "RTI TA" (modified Territorial Authority as used in production of the Regional Tourism Indicators) is in which genuine TA as used by Statistics New Zealand.
}
\usage{data(locations)}
\format{
 The SNZ_TA and RTI_TA variables are self explanatory.  The "Count" variable shows the number of area units in each RTI_TA.
 }
\details{
The RTI_TA variable matches the names of the polygons in TA_TRED@data
}
\source{
New Zealand Ministry of Business, Innovation and Employment
}
\references{
www.mbie.govt.nz
}

\seealso{
\code{\link{TA}}, \code{\link{TA_TRED}}
}

\examples{
head(TA_lookup)

}
\keyword{datasets}
