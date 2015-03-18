library("mbie", lib.loc = "P:/R/libraries/3.1.2")

tmp <- help.search("", package = "mbie", lib.loc = "P:/R/libraries/3.1.2")
tmp$matches[,"name"]
for(topic in tmp$matches[,"name"]){
  do.call(example, list(topic,package = "mbie", lib.loc = "P:/R/libraries/3.1.2"))
}


