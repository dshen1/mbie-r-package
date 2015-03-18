target.lib.loc <- "P:/R/libraries/3.1.2"

install.packages("P:/OTSP/data-infrastructure/archive/mbie_0.8.9.tar.gz", lib = target.lib.loc, repos = NULL, type = "source")


library("mbie", lib.loc = target.lib.loc)

tmp <- help.search("", package = "mbie", lib.loc = target.lib.loc)
topics <- tmp$matches[,"name"]
for(i in 25:length(topics)){
  (topic <- topics[i])
  do.call(example, list(topic,package = "mbie", lib.loc = target.lib.loc))
}



library("mbiedata", lib.loc = target.lib.loc)
library("mbiemaps", lib.loc = target.lib.loc)
