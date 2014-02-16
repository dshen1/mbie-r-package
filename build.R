
require(devtools)

# Set the library path so the Test folder is listed first and the installation will be to there,
# rather than to the folder used for libraries by everyone
.libPaths(c("P:/r/libraries/AAA TEST LIBRARIES", .libPaths()))


# Build the package:
setwd("pkg")
  build(path= "P:/OTSP/data-infrastructure/testing/", binary=TRUE)
setwd("..")
