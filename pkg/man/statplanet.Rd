\name{statplanet}
\alias{statplanet}
\title{
Convert data from long format to StatPlanet format
}
\description{
Takes a dataframe in long format, converts it to wide StatPlanet format with 11 specified columns followed by a column for each region in a map, and saves to a specified location.
}
\usage{
statplanet(long, file)
}
\arguments{
\item{long}{
A data frame with variables names category, subcategory, indicator, time, region and value.  Other columns will be ignored.  'region' should be the exact region ids used by StatPlanet.
}

\item{file}{
Filename to save the data to.  This is usually "data.csv" in the folder holding the shapefile read by StatPlanet and the StatPlanet.exe file - something like "C:/Users/Peter Ellis/Documents/StatPlanet_Plus/Shapefile_map_(ESRI)/data.csv"
}
  
}
\details{
This is very much an experimental function to help evaluate whether we want to use StatPlanet.  There are known issues - most important of which is that the csv output cannot be directly read by StatPlanet but for some reason (probably to do with file encoding) needs to be opened in Excel and saved again directly (without changing anything) as a csv.  This reduces the file size by about 35 per cent and makes it readable by StatPlanet.

Future development might include a more flexible use of categories, subcategories and subsubcategories.  At the moment the function expects all data to have a category and a subcategory, no more and no less.
}
\value{
NULL
}
\references{
\link{http://www.statsilk.com/software/statplanet}
}
\author{
Peter Ellis <peter.ellis@mbie.govt.nz>
}

\seealso{


}
\examples{
  \dontrun{
library(mbie)
data(RTEs)


long0 <- ddply(RTEs, .(Type, YEMar, Territorial_Authority), summarise,
               value = round(sum(Spend)))

long0$subcategory <- "Spend by origin (int/dom)"


names(long0) <- gsub("YEMar", "time", names(long0) )
names(long0) <- gsub("Type", "indicator", names(long0) )
names(long0) <- gsub("Territorial_Authority", "region", names(long0) )


long1 <- ddply(RTEs, .(Product, YEMar, Territorial_Authority), summarise,
              value = round(sum(Spend)))

long1$subcategory <- "Spend by product"


names(long1) <- gsub("YEMar", "time", names(long1) )
names(long1) <- gsub("Product", "indicator", names(long1) )
names(long1) <- gsub("Territorial_Authority", "region", names(long1) )

long2 <- ddply(subset(RTEs, Type=="International"), .(Origin, YEMar, Territorial_Authority), summarise,
               value = round(sum(Spend)))

long2$subcategory <- "Spend x international origin"


names(long2) <- gsub("YEMar", "time", names(long2) )
names(long2) <- gsub("Origin", "indicator", names(long2) )
names(long2) <- gsub("Territorial_Authority", "region", names(long2) )


long3 <- ddply(subset(RTEs, Type=="Domestic"), .(Origin, YEMar, Territorial_Authority), summarise,
               value = round(sum(Spend)))

long3$subcategory <- "Spend x domestic origin"


names(long3) <- gsub("YEMar", "time", names(long3) )
names(long3) <- gsub("Origin", "indicator", names(long3) )
names(long3) <- gsub("Territorial_Authority", "region", names(long3) )


long <- rbind(long0, long1, long2, long3)
long$category <- "Tourism spend"

file <- "C:/Users/Peter Ellis/Documents/StatPlanet_Plus/Shapefile_map_(ESRI)/data.csv"

LongToStatPlanet(long, file)


}
}

\keyword{ connection }

