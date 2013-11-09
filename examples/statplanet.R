library(mbie)
data(RTEs)
head(RTEs)

RTE2 <- subset(RTEs, !is.na(Territorial_Authority))

RTE3 <- dcast(RTE2,
              Product + YEMar ~ Territorial_Authority, 
              function(x){round(sum(x))}, value.var="Spend")

long <- ddply(RTE2, .(Product, YEMar, Territorial_Authority), summarise,
              value = round(sum(Spend)))

long$category <- "Product"


names(long) <- gsub("YEMar", "time", names(long) )
names(long) <- gsub("Product", "indicator", names(long) )
names(long) <- gsub("Territorial_Authority", "region", names(long) )

long2 <- ddply(subset(RTE2, Type=="International"), .(Origin, YEMar, Territorial_Authority), summarise,
               value = round(sum(Spend)))

long2$category <- "International origin"


names(long2) <- gsub("YEMar", "time", names(long2) )
names(long2) <- gsub("Origin", "indicator", names(long2) )
names(long2) <- gsub("Territorial_Authority", "region", names(long2) )

long <- rbind(long, long2)

file <- "C:/Users/Peter/Documents/StatPlanet_Plus/Shapefile_map_(ESRI)/data2.csv"
LongToStatPlanet(long, file)