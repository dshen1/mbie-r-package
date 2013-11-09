require(devtools)

setwd("pkg")

# PE laptop environemnt only
# setwd("C:/Users/Peter Ellis/Documents/GitHub/mbie/pkg")

unlink( '../output', TRUE)
dir.create("../output", showWarnings=FALSE)
build(path= "../output")
build(path= "../output", binary=TRUE)

# next lines specific to Peter's home environment
detach("package:mbie", unload=TRUE)
install.packages("../output/mbie_0.6.0.zip", repos=NULL, type="win.binary")
library(mbie)