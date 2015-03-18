library("mbie", lib.loc = "P:/R/libraries/3.1.2")

tmp <- help.search("", package = "mbie", lib.loc = "P:/R/libraries/3.1.2")
topics <- tmp$matches[,"name"]
for(i in 15:length(topics)){
  (topic <- topics[i])
  do.call(example, list(topic,package = "mbie", lib.loc = "P:/R/libraries/3.1.2"))
}



library("mbiedata", lib.loc = "P:/R/libraries/3.1.2")
library("mbiemaps", lib.loc = "P:/R/libraries/3.1.2")
