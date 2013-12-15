<img src="figure/Header.png" title="" alt="" style="display: block; margin: auto;" />

"mbie" R package
==================

This public repository holds source code and data for the "mbie" R package used by New Zealand's Ministry of Business, Innovation and Employment, primarily but not only the Sector Performance team.  The package is public to allow access to the data under the conditions of the most transparency and to the maximum degree of granularity, as directed by the New Zealand government.  Over 2014 further improvements will be made to make the data more accessible.

This data and code are available under a Creative Commons license - see http://ict.govt.nz/guidance-and-resources/information-and-data/nzgoal/ for details.


## Demo of Regional Tourism Estimates data


```r

data(RTEs)

require(treemap)

# Estimate compound annual growth rate
growth <- ddply(RTEs, .(Territorial_Authority, Region), summarise,
                growth=CAGR(sum(Spend[YEMar==2013]) / sum(Spend[YEMar==2009]), 4))

# Merge with the absolute size of tourism in 2013
RTE2 <- merge(ddply(subset(RTEs, YEMar==2013), .(Territorial_Authority, Region), summarise,
                    Spend=sum(Spend)),
              growth)

# Draw treemap to show size and growth by hierarchical TA and Region
treemap(RTE2,
        index=c("Region", "Territorial_Authority"),
        vSize="Spend",
        vColor="growth",
        palette="Spectral",
        title="Regional Tourism 2013, and CAGR 2009 to 2013",
        type="value",
        inflate.labels=TRUE,
        algorithm="squarified")
```

<img src="figure/RTESeg.png" title="plot of chunk RTESeg" alt="plot of chunk RTESeg" style="display: block; margin: auto;" />



## Demo of International Visitor Survey data
This includes use of the backcasting "Adjusted Factor" from the redevelopment in 2013, and also illustrates the CountryGroup() and stat_sa() (for seasonal adjustment on the fly) functions.

```r

library(plyr)
library(scales)
library(ggplot2)
library(ggthemes)

data(IVStrips)
tmp <- ddply(IVStrips, .(YearQuarter, CountryGroup(COPRTop5)), summarise, Total= sum(SmoothTotalSpend * FinalWeight * Adjusted_Factor))
tmp$period <- with(tmp, as.numeric(substring(YearQuarter, 1, 4)) + (as.numeric(substring(YearQuarter, 6, 6))-.5)/4)
names(tmp)[2] <- "Country"

ggplot(tmp, aes(x=period, y=Total / 10^6, color=Country)) +
  stat_sa(frequency=4, geom="point", size=2) +
  stat_sa(frequency=4, size=2) +
  geom_line(frequency=4, size=.8, alpha=.4) +
  theme_economist() +
  labs(x="", y="Total spend ($m)\n") +
  scale_color_manual("", values=tourism.cols("Alternating")) +
  ggtitle("Seasonally adjusted NZ tourism spend by country of origin")
```

<img src="figure/IVSeg.png" title="plot of chunk IVSeg" alt="plot of chunk IVSeg" style="display: block; margin: auto;" />



## Demo of Regional Tourism Estimates in combination with maps in the accompanying mbiemaps package


```r

# load in RTO map
library(sp)
library(lattice)
library(latticeExtra)
library(mbiemaps)
data(RTO)

# load in the Regional Tourism Estimate (RTE) data
data(RTEs)

# Aggregate domestic tourism spend and average annual growth rate
dom <- ddply(subset(RTEs, Type == "Domestic"), .(RTO), summarise,
  Growth = CAGR(sum(Spend[YEMar==2013]) / sum(Spend[YEMar==2009]), 4),
  Spend2013 = sum(Spend[YEMar==2013]))
	

# merge the RTE with the data from the data slot of the RTO map
data2 <- merge(RTO@data, dom, by="RTO")

# replace the data slot of our RTO map with the new data frame
RTO@data <- data2


#----------------draw map - fancy version - title, palette, border colour, added circle layer---------------

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
	sp.layout=list("sp.points", coords, pch=1, col="black", lwd=2, cex=sizes)) # add circles layer
grid.text("Circle size is proportional to\ndomestic tourism spend in 2013", .2,.7)
grid.text("Average growth per year in domestic tourism 2009 - 2013", .93,.5, , rot=-90)
```

<img src="figure/unnamed-chunk-1.png" title="plot of chunk unnamed-chunk-1" alt="plot of chunk unnamed-chunk-1" style="display: block; margin: auto;" />

```r

```

