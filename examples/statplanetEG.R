library(mbie)
data(RTEs)
head(RTEs)

RTE2 <- subset(RTEs, !is.na(Territorial_Authority))

RTE3 <- dcast(RTE2,
              Product + YEMar ~ Territorial_Authority, 
              function(x){round(sum(x))}, value.var="Spend")

long0 <- ddply(RTE2, .(Type, YEMar, Territorial_Authority), summarise,
               value = round(sum(Spend)))

long0$subcategory <- "Spend by origin (int/dom)"


names(long0) <- gsub("YEMar", "time", names(long0) )
names(long0) <- gsub("Type", "indicator", names(long0) )
names(long0) <- gsub("Territorial_Authority", "region", names(long0) )


long1 <- ddply(RTE2, .(Product, YEMar, Territorial_Authority), summarise,
              value = round(sum(Spend)))

long1$subcategory <- "Spend by product"


names(long1) <- gsub("YEMar", "time", names(long1) )
names(long1) <- gsub("Product", "indicator", names(long1) )
names(long1) <- gsub("Territorial_Authority", "region", names(long1) )

long2 <- ddply(subset(RTE2, Type=="International"), .(Origin, YEMar, Territorial_Authority), summarise,
               value = round(sum(Spend)))

long2$subcategory <- "Spend x international origin"


names(long2) <- gsub("YEMar", "time", names(long2) )
names(long2) <- gsub("Origin", "indicator", names(long2) )
names(long2) <- gsub("Territorial_Authority", "region", names(long2) )


long3 <- ddply(subset(RTE2, Type=="Domestic"), .(Origin, YEMar, Territorial_Authority), summarise,
               value = round(sum(Spend)))

long3$subcategory <- "Spend x domestic origin"


names(long3) <- gsub("YEMar", "time", names(long3) )
names(long3) <- gsub("Origin", "indicator", names(long3) )
names(long3) <- gsub("Territorial_Authority", "region", names(long3) )


long <- rbind(long0, long1, long2, long3)
long$category <- "Tourism spend"

file <- "C:/Users/Peter Ellis/Documents/StatPlanet_Plus/Shapefile_map_(ESRI)/data.csv"
LongToStatPlanet(long, file)