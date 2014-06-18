\name{locations_2013}
\alias{locations_2013}
\alias{locations}
\docType{data}
\title{
locations_2013
}
\description{
Coordinates of 483 locations used in the  International Visitor Survey from July 2013 onwards
}
\usage{data(locations_2013)}
\format{
 Most of the variables are self explanatory but there are some quirks.  "RTO" is not exactly the same as in other data objects in this package. "TFA" stands for "Tourism Flows Area", a custom classification used for the Tourism Flows Model.  RTO.code and Region.code are specific to this dataset and are included only for historical reasons.  NZMG (north and east) is an obsolete New Zealand coordinate system; NZTM is the up to date one.  The latitude and longitude were projected from NZTM by means of \code{proj4::project(list(x=locations$NZTM.E , y=locations$NZTM.N), proj=pr, inverse=T)}, where \code{pr="+proj=tmerc +lat_0=0.0 +lon_0=173.0 +k=0.9996 +x_0=1600000.0 +y_0=10000000.0 +datum=WGS84 +units=m"}
 }
\details{
These locations are different to those used in the old IVS (pre July 2013), see locations_old for these.
}
\source{
New Zealand Ministry of Business, Innovation and Employment
}
\references{
www.mbie.govt.nz
}

\seealso{
\code{\link{IVSvisits}}, \code{\link{DTSvisits}}
}

\examples{

THIS IS AN OLD EXAMPLE BASED ON OLD IVS.  NEEDS UPDATING THIS WITH A RELEVANT EXAMPLE FOR NEW IVS LOCATIONS (E.G. NZ MAP SHOWING INTENSITY OF VISITS IN THE VARIOUS RTOS)
require(ggplot2)
data(locations_2013)
data(IVSvisits)

# Create a subsample of just 10000 "visits" (legs) from IVSvisits
set.seed(123)
visits2 <- IVSvisits[sample(1:nrow(IVSvisits), 10000), c(4:44,196:199,204:205)]

# Merge with locations at beginning and end of each visit
visits3 <- merge(visits2, locations_2013[ , c("Location","Longitude","Latitude")], by.x="VisitDestinationPlace", by.y="Location")
visits4 <- merge(visits3, locations_2013[ , c("Location","Longitude","Latitude")], by.x="VisitOriginPlace", by.y="Location",
                 suffixes = c("_Destination", "_Origin"))

# Draw plot
ggplot() +
  theme_bw() +
  coord_map() +
  borders("nz") +
  geom_segment(data=visits4, 
               aes(x=Longitude_Origin,
                   y=Latitude_Origin,
                   xend=Longitude_Destination,
                   yend=Latitude_Destination,
                   size=FinalWeight),
               alpha=0.05) +
  facet_wrap(~POVTop3)


}
\keyword{datasets}
