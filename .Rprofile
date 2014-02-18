 
 # the usual command to point the libraries at the right spot
 .libPaths(c(.libPaths(), "P:/R/libraries/3.0.1"))

 # Set the library path so the Test folder is listed first and the installation will be to there,
# rather than to the folder used for libraries by everyone
.libPaths(c("P:/r/libraries/AAA TEST LIBRARIES", .libPaths()))


 library(utils)
 library(Defaults)
 library(mbie)
 library(mbiedata)
 
 setDefaults(q, save="no")
 useDefaults(q)
 Sys.setenv(PDFLATEX="xelatex")
 setInternet2(TRUE) 
 Sys.setenv(R_GSCMD = '"C:/Program Files (x86)/gs/gs8.54/bin/gswin32c.exe"')
 Sys.setenv(SDMX_CONF = "P:/R/configuration.properties")


