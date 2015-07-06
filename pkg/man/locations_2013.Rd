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
require(ggplot2)
library(mbiedata)
data(IVSvisits)
data(locations_2013)
library(mbiemaps)
data(region_simpl_gg)
library(dplyr)

# merge 2013 data from IVS visits with location information
visits <-  merge(IVSvisits[IVSvisits$InterviewYear == 2013, c("IDNumber", "VisitOriginPlace", "FinalWeight", "POVArrival")],
                  locations_2013, 
                  by.x = "VisitOriginPlace", by.y = "IVS1_Label")

# summarise visits by region and remove duplicates

visits2 <- visits \%>\%
            distinct(IDNumber, Region) \%>\%
            group_by(Region) \%>\%
            summarise(
               Visitors = sum(FinalWeight),
               Sample = length(FinalWeight)
               )

# merge onto map data
visits3 <- merge(visits2, region_simpl_gg ,by.x="Region", by.y="NAME", all.y=TRUE)

# Draw plot
ggplot(visits3, aes(x = long, y = lat, group = group, fill = Visitors)) +
   geom_polygon(colour = "grey60") +
   theme_nothing() + scale_fill_continuous(label=comma, low = "white", high = "blue") +
   labs(title="Visitors by Region in 2013")
  
}
\keyword{datasets}
