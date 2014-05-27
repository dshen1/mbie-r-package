##  Purpose:  Creates locations_old.rda and locations_2013.rda
##			in the past there was just the locations.rda.  We are replacing this with locations_old 
##			and adding a new locations_2013.rda for the uptodate locations in IVS2
##	Developed by: Antonia Milkop
##	Date: 27 May 2014
##	Peer reviewed:

#one this script has been run once, this next line won't work because we have replaced locations with locations_old (i.e. deleted locations)
data(locations)
save(locations, file="raw_data/locations.rda")

##		add another field to identify whether data is from locations1 or locations2 (i.e. IVS1 or IVS2)
##  	currently locations has 2500 ish rows.  locations2 has another 300 odd.  So we'll have about 3000 rownames in the end

#bring in the new 'locations2' list of IVS2 location names
locations_old <- locations
locations_2013 <- read.csv("raw_data/IVS2_location_26052014.csv")

save(locations_old, file = "pkg/data/locations_old.rda")
save(locations_2013, file = "pkg/data/locations_2013.rda")

## Next steps:
## 	1. Delete the existing 'locations.rda' file in pkg/data
##  2. run the build.R file , this will 'build' your new package with these new data files in
##  3. Document what you've done in the help files
##			pkg/man you'll need two .Rd files, locations_old.Rd and locations_2013.Rd and delete the old locations.Rd file
##			pkg/man/mbie-package.Rd
##			pkg/"DESCRIPTION"
##			pkg/"NEWS"

