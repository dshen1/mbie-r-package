############################################################################
## prepare package building environment ------------------------------------
require(devtools)

# Note - set the library path so the Test folder is listed first and the installation will be to there,
# rather than to the folder used for libraries by everyone.  This should be done in the .Rprofile
# but in case not you can always run the line below:
.libPaths(c("P:/r/libraries/AAA TEST LIBRARIES","P:/R/libraries/3.1.2", .libPaths()))
############################################################################



############################################################################
## build, install and load package in test folder --------------------------
## Build the binary package in the test folder
build(pkg ="pkg", path= "P:/r/libraries/AAA TEST LIBRARIES", binary=TRUE)

## install the package to test folder
install.packages("P:/r/libraries/AAA TEST LIBRARIES/mbie_0.9.0.zip",repos = NULL,lib = "P:/r/libraries/AAA TEST LIBRARIES")

## load the package from test folder
library(package = "mbie", lib.loc = "P:/r/libraries/AAA TEST LIBRARIES")
############################################################################



############################################################################
## Test the package --------------------------------------------------------
## after load the wroking version, go though the examples of newly added fucntions and check if they work properly.
############################################################################



############################################################################
## deploy to production ----------------------------------------------------
## After review, we are ready to deploy the package to production

# ## Build the package and save it into archive folder
# build(pkg ="pkg", path= "P:/OTSP/data-infrastructure/archive", binary=TRUE)
# build(pkg ="pkg", path= "P:/OTSP/data-infrastructure/archive", binary=FALSE)
# 
# ## install the package to production libraries.
# install.packages("P:/OTSP/data-infrastructure/archive/mbie_0.9.0.zip",repos = NULL,lib = "P:/R/libraries/3.1.2")
############################################################################
