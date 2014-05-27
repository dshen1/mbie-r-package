##  Purpose:  	Tests the changes we have made to IVS2 locations. 
##				changes are shown here:  grooming/Importing IVS2 locations into locations file.R 
##	Developed by: Antonia Milkop
##	Date: 27 May 2014
##	Peer reviewed:

setwd("P:/R/libraries/AAA TEST LIBRARIES/mbie")


## check to see if help files that we have updated have indeed changed 
?locations_old
?locations_2013

#I made an alias in locations_2013 so that if someone looks for old locations data/help (i.e. types ?locations), they'll now get redirected to locations_2013
?locations 

##############################################################################
?mbie-package #THIS DOESN'T LINK TO THE MBIE-PACKAGE.Rd HELPFILE - WHY?
##############################################################################
?mbie #THIS DOES WORK HOWEVER

##	 check to see if examples are working
##	first we have to load appropriate data and the data we've just amended or changed
library(ggplot2)
data(IVSvisits)
data(locations_2013)
data(locations_old)

str(locations_2013) #483 observations, 24 variables. 
str(locations_old) #2531 observations, 19 variables

##
example(locations_old)  #working ok

example(locations_2013) #now working ok (needed to change R script in man/locations_2013.Rd)
example(locations)