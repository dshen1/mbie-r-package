require(devtools)

setwd("pkg")

# PE laptop environemnt only
# setwd("C:/Users/Peter Ellis/Documents/GitHub/mbie/pkg")

unlink( '../output', TRUE)
dir.create("../output", showWarnings=FALSE)
build(path= "../output")
build(path= "../output", binary=TRUE)

