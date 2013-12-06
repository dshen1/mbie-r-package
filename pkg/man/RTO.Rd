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
Mapping seems to work better in an R console (eg in conjunction with Notepad++) than in RStudio.  RStudio is very slow to render maps.
}

\seealso{
\code{\link{IVSvisits}}, \code{\link{IVSspend}}
}

\examples{
#==============Example 1, basic plot of RTO and TA using base graphics===================
require(sp)
data(RTO)
data(TA)
par(fg=NA)
plot(RTO, col=tourism.cols("Alternating"))
par(fg="black")
plot(TA, add=TRUE)
axis(1)
axis(2)

#==================Example 2, choropleth map of RTOs using lattice package==================

# load in RTO map
library(sp)
library(lattice)
library(latticeExtra)
data(RTO)

# load in some Regional Tourism Estimate (RTE) data
data(RTEs)
head(RTEs)
dom <- ddply(subset(RTEs, Type == "Domestic"), .(RTO), summarise,
  Growth = CAGR(sum(Spend[YEMar==2013]) / sum(Spend[YEMar==2009]), 4),
	Spend2013 = sum(Spend[YEMar==2013]))
	

# merge the RTE with the data from the data slot of the RTO map
data2 <- merge(RTO@data, dom, by="RTO")

# check order was preserved ok and is same as the original RTO@data
cbind(RTO@data, data2)

# replace the data slot of our RTO map with the new data frame
RTO@data <- data2



#-------------draw map simple version with all the defaults--------------
# We only need to say which variable in the RTO@data slot to use for the colours
spplot(RTO, zcol="Growth")


#----------------draw map Fancier version - title, palette, border colour, fonts, added circle layer---------------
library(extrafont)

# defining our own palette
# First, how many colours are needed that are in the negative band
lessthanzero <- round((-min(RTO@data$Growth)) / sum(abs(range(RTO@data$Growth))) * 100)

# then create a palette of colours from red to grey and out to MBIE blue
cols <- c(colorRampPalette(c("red", "grey95"))(lessthanzero), colorRampPalette(c("grey95", mbie.cols(1)))(100-lessthanzero))

# coordinates to add circles
coords <- SpatialPoints(coordinates(RTO))

# how big will the circles be?  "sizes" variable for later use
sizes <- sqrt(RTO@data$Spend2013)/8

# set parameters so no axis line ie no box around the plot
trellis.par.set("axis.line", list(col=NA,lty=1,lwd=1))

spplot(RTO, zcol="Growth", col.regions=cols, main="Domestic tourist spend", col="white", 
	sp.layout=list("sp.points", coords, pch=1, col="black", lwd=2, cex=sizes), # add circles layer
	par.settings=list(grid.pars=list(fontfamily="Georgia")))
grid.text("Circle size is proportional to\ndomestic tourism spend in 2013", .2,.7, gp=gpar(fontfamily="Calibri"))
grid.text("Average growth per year in domestic tourism 2009 - 2013", .93,.5, , rot=-90, gp=gpar(fontfamily="Calibri"))

}
\keyword{datasets}
