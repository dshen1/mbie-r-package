\name{RTEs}
\alias{RTEs}
\alias{RTEs}
\docType{data}
\title{
Regional Tourism Estimates
}
\description{
Regional Tourism Estimates
}
\usage{data(RTEs)}
\format{
 Very simple...
 
 }
\details{
%%  ~~ If necessary, more details than the __description__ above ~~
}
\source{
New Zealand Ministry of Business, Innovation and Employment
}
\references{
www.mbie.govt.nz
}

\seealso{

}

\examples{

#------Example 1 - stacked bar chart, pyramid style----------------

require(extrafont)
require(plyr)
require(scales)
require(ggplot2)


data(RTEs)

# Remove the "RTO" from the end of each Regional Tourism Organisation name
rte_orig <- RTEs
rte_orig$RTO2 <- with(rte_orig, gsub(" RTO", "", as.character(RTO)))


# Create the version aggregated by RTO. 
# The /4 is there so we get an average annual spend over the four years.
tmp <- ddply(rte_orig, .(RTO2, Product, Type), summarise,
             Spend=sum(Spend)/4)


# Create a vector to help us sort the RTOs to make the charts more meaningful
for_sorting <- ddply(rte_orig, .(RTO2), summarise, Spend=sum(Spend))

# .... and sort the order of levels, which flows through to how they appear
# in ggplot charts.
tmp$RTO2 <- factor(tmp$RTO2, levels=for_sorting[order(for_sorting$Spend), 1])

# Define common palette and order of colours for mapping to ANZSIC.  Do this
# just once here so easy to change for all the plots at once.  
Cols <- mbie.cols(c(6,5:1))

# set font family for plots
theme_set(theme_grey(base_family="Georgia"))

# Pyramid stacked barchart
# Note the very cool trick with two calls to geom_bar, thanks to Stack Exchange.
# Also note we then need to relabel the negative points on the horizontal axis
# (which is "y", because of the coord_flip)

# Note need to divide by 1000 to turn it into billions

ggplot(tmp, aes(x=RTO2, weight=Spend/1000, fill=Product)) +
  geom_bar(subset=.(Type=="International")) + 
  geom_bar(subset=.(Type=="Domestic"), aes(y=..count..*(-1))) + 
  coord_flip() +
  scale_fill_manual(values=Cols) +
  scale_y_continuous(breaks=seq(-3,3,1), labels=abs(seq(-3,3,1))) +
  geom_hline(yintercept=0, color="white", size=1) +
  annotate(geom="text", x=3, y=-5/4, label="Domestic", color="grey20", family="Georgia") +
  annotate(geom="text", x=3, y=5/4, label="International", color="grey20", family="Georgia") +
  labs(y="\nAverage annual spend 2008-2012 ($ billion)", x="",
       title="Tourism spend by Regional Tourism Organisation\n(sorted by size of total tourism)\n")

#-----------------Example 2 - weighted network graph of regional relations----------------


if(require(igraph)==TRUE){

  data(RTEs)
  
  # Create data frame of "edges" - attributes by pairwise comparisons
  RTE_sub <- ddply(RTEs[RTEs$Type=="Domestic", ], .(Origin, Region), summarise, Spend=sum(Spend))
  # remove travel to own region
  RTE_sub <- subset(RTE_sub, as.character(Origin) != as.character(Region))
  
  # Create a data frame of the vertices - one row per Region
  RTE_v <- ddply(RTE_sub, .(Region), summarise, total=sum(Spend))
  
  # A palette of colours
  ramp <- colorRampPalette(c("grey95", "red"))( 100)
  
  # create a distances matrix to use later in multidimensional scaling to set out location of vertices
  distances <- 1 / (acast(RTE_sub, Origin ~ Region, sum, value.var="Spend")+50)
  
  # create the basic network plot
  g <- graph.data.frame(RTE_sub, directed=TRUE)
  
  # draw the basic plot
  plot(g, vertex.shape="sphere", layout=cmdscale(distances), 
    	vertex.color=ramp[as.numeric(cut(RTE_v$total, 100))],
  		vertex.size=sqrt(RTE_v$total)/4, vertex.label.family="Calibri", vertex.label.font=2,
  		edge.arrow.size=.8, edge.color=ramp[as.numeric(cut(RTE_sub$Spend, 100))],
  		edge.width=sqrt(RTE_sub$Spend)/3)
  
  # 3d interactive version
  rglplot(g, vertex.shape="sphere", layout=cmdscale(distances, 3), 
          vertex.color=ramp[as.numeric(cut(RTE_v$total, 100))],
          vertex.size=sqrt(RTE_v$total)/4, vertex.label.color="lightblue",
          edge.arrow.size=.8, edge.color=ramp[as.numeric(cut(RTE_sub$Spend, 100))],
          edge.width=sqrt(RTE_sub$Spend)/3)

}

#-------------------example with treemap---------------------
# Load in RTE data and libraries needed
data(RTEs)

require(treemap)
require(extrafont)

# Estimate compound annual growth rate
growth <- ddply(RTEs, .(Territorial_Authority, Region), summarise,
                growth=CAGR(sum(Spend[YEMar==2012]) / sum(Spend[YEMar==2009]), 3))

# Merge with the absolute size of tourism in 2012
RTE2 <- merge(ddply(subset(RTEs, YEMar==2012), .(Territorial_Authority, Region), summarise,
                    Spend=sum(Spend)),
              growth)

# Draw treemap to show size and growth by hierarchical TA and Region
treemap(RTE2,
        index=c("Region", "Territorial_Authority"),
        vSize="Spend",
        vColor="growth",
        palette="Spectral",
        title="Regional Tourism 2012, and CAGR 2009 to 2012",
        font.title="Georgia",
        font.labels="Calibri",
        font.legend="Georgia",
        type="value",
        inflate.labels=TRUE,
        algorithm="squarified")


}
\keyword{datasets}
