### 	This .rprofile has the TEST folder listed first and THEN the usual command folder
##		When you load a package it looks in each library location in order and the first time it finds
##		the sought after package it loads it.  
##		For the purposes of testing the new build we want R to load the TEST libraries first


 # Set the library path so the Test folder is listed first and the installation will be to there,
# rather than to the folder used for libraries by everyone
.libPaths(c("P:/r/libraries/aaa test libraries", "P:/r/libraries/current"))

##  .libPaths(c("P:/r/libraries/AAA TEST LIBRARIES", .libPaths()))
 
 # the usual command to point the libraries at the right spot
 ##  .libPaths(c("P:/R/libraries/current",.libPaths()))


 library(utils)
 library(Defaults)
#  library(mbie)
#  library(mbiedata)
 
 setDefaults(q, save="no")
 useDefaults(q)
 Sys.setenv(PDFLATEX="xelatex")
 setInternet2(TRUE) 
 Sys.setenv(R_GSCMD = '"C:/Program Files (x86)/gs/gs8.54/bin/gswin32c.exe"')
 Sys.setenv(SDMX_CONF = "P:/R/configuration.properties")


