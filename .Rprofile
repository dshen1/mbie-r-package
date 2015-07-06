###    This .rprofile has the TEST folder listed first and THEN the usual command folder
##		When you load a package it looks in each library location in order and the first time it finds
##		the sought after package it loads it.  
##		For the purposes of testing the new build we want R to load the TEST libraries first

## load common .Rprofile
source("P:/R/common.Rprofile")

## Set the library path so the Test folder is listed first and the installation will be to there,
## rather than to the folder used for libraries by everyone
.libPaths(c("P:/R/libraries/AAA TEST LIBRARIES", .libPaths()))

## delete the package folder from test folder when quiting R, to avoid write permission conflicts
.Last <- function(){ 
   unlink("P:/R/libraries/AAA TEST LIBRARIES/mbie", recursive = TRUE, force = TRUE)
   unlink("P:/R/libraries/AAA TEST LIBRARIES/mbie_*", recursive = TRUE, force = TRUE)
}