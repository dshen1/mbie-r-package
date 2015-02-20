
require(devtools)

# Note - set the library path so the Test folder is listed first and the installation will be to there,
# rather than to the folder used for libraries by everyone.  This should be done in the .Rprofile
# but in case not you can always run the line below:
.libPaths(c("P:/r/libraries/AAA TEST LIBRARIES", "P:/R/libraries/current",.libPaths()))


# Build the package:
build(pkg ="pkg", path= "P:/OTSP/data-infrastructure/archive/", binary=TRUE)
build(pkg ="pkg", path= "P:/OTSP/data-infrastructure/archive/", binary=FALSE)

## load pacakge
rm(list=ls())
if("package:mbie" %in% search()) detach("package:mbie")
library(mbie,lib.loc="P:/r/libraries/AAA TEST LIBRARIES")