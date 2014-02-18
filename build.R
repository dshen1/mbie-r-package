
require(devtools)

# Note - set the library path so the Test folder is listed first and the installation will be to there,
# rather than to the folder used for libraries by everyone.  This should be done in the .Rprofile
# but in case not you can always run the line below:
.libPaths(c("P:/r/libraries/AAA TEST LIBRARIES", .libPaths()))


# Build the package:
setwd("pkg")
  build(path= "P:/OTSP/data-infrastructure/testing/", binary=TRUE)
setwd("..")
