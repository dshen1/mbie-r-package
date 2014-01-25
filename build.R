require(devtools)

setwd("pkg")

# PE laptop environment only
build(path= "C:/Users/Peter Ellis/Dropbox/Packages")
build(path= "C:/Users/Peter Ellis/Dropbox/Packages", binary=TRUE)

setwd("..")
