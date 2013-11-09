#### more documentation up here
# function definition:
#	- creategraph() 
#	- autocheck()
#
########################################
# function definition:
#######################################


creategraph<-function (x.df, area, RTItype, fpath){
		
	# The function creategraph creates 2 graphs (top and bottom) in a png file for each 
	# levels or values of a vector of character strings (it could be country names, RTO names or ANZSIC categories).    
	# The function creates a subfolder if it doesn't exist and saves the png files in it.
	# The function does not return output.

	#Parameters:
	#x.df = a data frame with transactions or spend data 
	#area = a character string in quote which identify the column of Country names or RTO names or ANZSIC categories
	#RTItype = a character string in quote to use as a prefix for folder names and output names to help to identify series "INTNL"=international or "DMSTC"=Domestic
	#fpahth = a character string in quote: folder path to locate where outputs are going to be saved. 

		
	#Create a subfolder  
	dir.create(file.path(fpath, paste(format(Sys.time(), "%Y%m%d"), " ", RTItype," ",
		substring(area,1, ifelse(regexpr("_", area)[1]==-1, nchar(area), regexpr("_", area)[1]-1)),sep="")))
	#Complete path to the new subfolder
	subf<-paste("/",format(Sys.time(), "%Y%m%d") ," ", RTItype," ", 
			substring(area,1, ifelse(regexpr("_", area)[1]==-1, nchar(area), regexpr("_", area)[1]-1)),"/", sep="")

		 
	#Replace missing value by 0 
	x.df$TRANSACTIONS[is.na(x.df$TRANSACTIONS)]<-0
	x.df$SPEND[is.na(x.df$SPEND)]<-0
	
	cyc<-levels(as.factor(x.df[,area]))

	#Loop that creates the graphs (line plots) on for each country
	# The loop creates a line plot graph for each country/or RTO/or ANZSIC based on the transaction count variable for jan 2008 to the lastest month available. 
	for (i in 1:(length(cyc))){
		tmp<- subset(x.df, x.df[,area]==cyc[i])

		#str_replace_all() function is set to remove all special characters such as . ' : or parentheses to avoid error in png file name.
		png(filename=paste(fpath, subf, RTItype," ", str_replace_all(cyc[i], "[^[:alnum:]]", " "),".png", sep=""), 1200,600,res=80) 
			par(mfrow=c(2,1))
			par(mar=c(4,6,3,0))
			par(oma=c(1,1,2,1))
	
			plotG(tmp$PERIOD, tmp$TRANSACTIONS, type="l", xlab="Time", ylab="Count", 
			col =tourism.cols(2), main=paste(RTItype, " Total transaction count ",cyc[i],"\n", sep=""), cex.lab=.85, cex.axis=.85, cex.main=.85)

			plotG(tmp$PERIOD, tmp$SPEND, type="l", xlab="Time", ylab="Spend in NZ$", 
			col =tourism.cols(2), main=paste(RTItype, " Total spend ",cyc[i],"\n", sep=""), cex.lab=.85, cex.axis=.85, cex.main=.85 )	
		dev.off()
	}
} # END of creategraph function 
#######################################################################################################################



autocheck<- function(x.df, area, varn, RTItype, fpath){
	
	#The function creates a subfolder if it doesn't exist and saves the png and csv files in it.
	#The function SEARCH FOR OUTLIERS BY modelling time series and forecasting the last month and then by comparing to the actual value
	#If there is any, look closer to the suspicious value(s)(=lower or greater to the confidence interval given by holtWinters.prediction()) 
	#If there is a least one potential outlier, the function creates also a graph to visualize the distance between the outlier and confidence interval given by the forecast. 
	#The function creates a csv file with information about potential outlier(s) if there is none the csv file will content message "No outlier detected"
 	#For country series the function tag countries listed in the top 30 NZ tourism markets according to the IVA year ended June 2012.
	
	
	
	#Create a subfolder  
	dir.create(file.path(fpath, paste(format(Sys.time(), "%Y%m%d"), " ", RTItype," ",
		substring(area,1, ifelse(regexpr("_", area)[1]==-1, nchar(area), regexpr("_", area)[1]-1)), sep="")))
	
	# Complete path to the new subfolder
	subf<-paste("/",format(Sys.time(), "%Y%m%d")," ", RTItype," ", 
			substring(area,1, ifelse(regexpr("_", area)[1]==-1, nchar(area), regexpr("_", area)[1]-1)) , "/", sep="")


	#Replace missing value by 0 
	x.df[,varn][is.na(x.df[,varn])] <- 0

		
	vname <- levels(as.factor(x.df[,area]))
	l <- length(vname)
	#Create a empty list object
	tmp.l <- list()

	# Loop that creates forecast for each countrY
	#In the loop:
	# step 1 : subscript x.df dataframe (-create and temporary dataframe tmp) and create a timeserie object based on transaction or spend.
	# step 2 : Remove the last month of the ts and modelised the ts using  HoltWinters function
	# step 3 : use prediction function to predict 1 month and an confidence interval (at 95%)
	# step 4 : compare the actual transaction count to the lower and upper bounds of the predicted IC   
	#	Save the results with extra information in the list object tmp.l :
	#	- area name
	#	area name, actual transaction/Spend value, lower bound value, upper bound value, result test of (actual transaction count>=lower bound value),
	#	result test of (actual transaction count=<upper bound value), ratio (Actualvalue-predicted)*4/(CI upper bound - CI lower bound).

	for (i in 1:l){
		# i <- 133 # for debugging - Madagascar, for Sept 2013 data
		tmp <- subset(x.df, x.df[,area]==vname[i])
		lgth <- length(tmp[,1])
		tmp.ts <- ts(tmp[,varn], start=c(2008,1), frequency=12)
		tmp2.ts <- window(tmp.ts, end=c(tmp$YEAR[lgth-1], tmp$MONTH[lgth-1]), frequency=12) #Retrieve the last month 



		# cat(vname[i], "\n") # for debug 
		try(m <- HoltWinters(tmp2.ts, start.periods=3), silent=TRUE)
		if(class(.Last.value)=="try-error"){
			try(m <- HoltWinters(tmp2.ts, start.periods=4), silent=TRUE)
		}
		if(class(.Last.value)=="try-error"){
			try(m <- HoltWinters(tmp2.ts, start.periods=2), silent=TRUE)
		}		

		p <- predict(m, 1, prediction.interval = TRUE)
		
		tmp.l[[vname[i]]] <- c(vname[i], tmp[lgth,varn], round(p[1],2),round(p[3],2), round(p[2],2), 
							(tmp[lgth,varn] >= p[3] &  tmp[lgth,varn] <= p[2]),
		ifelse(tmp[lgth,varn] <= p[2], "Lower", "Higher"), round((tmp[lgth, varn]-p[1])*4/(p[2] - p[3]),2))
	}
	#Convert the list into a data frame
	ctr.df <- data.frame(matrix(unlist(tmp.l), nrow=l, byrow=T))

	# Convert the variables that should be numeric back to numeric type
	ctr.df[,c(2:5,8)] <- apply(ctr.df[,c(2:5,8)], 2, function(x){as.numeric(as.character(x))})

	#rename the columns:
	names(ctr.df)[1:8] <- c(area, varn, "FIT", "LOWER_BOUND", "UPPER_BOUND", "VALUE_IN_CI", "POSITION", "CRITERIA" )	
		

	#Take closer look a value which are not inside the IC
	suspicious <-subset(ctr.df, ctr.df$VALUE_IN_CI==F | abs(ctr.df$CRITERIA)>2)
	#suspicious <-ctr.df

	if (dim(suspicious)[1]==0) {
		#add a line in the empty data frame suspicious
		suspicious.sort<-rbind(suspicious,do.call(data.frame,setNames(as.list(c("No outlier detected",NA,NA,NA,NA,NA,NA,NA)), names(suspicious))))
	}	else {
		#Compute the difference ratio between the actual value and the lower bound when the actual value is < to the LOWER_BOUND
		#otherwise compute the difference ratio between the actual value and the upper bound.
		#suspicious$WRONG <-  ifelse(suspicious[, varn] < suspicious$LOWER_BOUND, 
		#		(suspicious[, varn]/suspicious$LOWER_BOUND-1), (suspicious[, varn]/suspicious$UPPER_BOUND-1))
		


		# Create a graph to visualize outliers 
		suspicious2 <- suspicious
		names(suspicious2)[1:2] <- c("AREA", "VARN") #TRICK to avoid error in aes, because aes() does not handle strings with ""
		# and could not use string_aes() because does not deal with reorder() function. 
		



		png(filename=paste(fpath, subf, RTItype,"  ", substring(area,1, ifelse(regexpr("_", area)[1]==-1, nchar(area), regexpr("_", area)[1]-1)), " ", varn," potential outliers",".png", sep=""), 6000,5000,res=600)
			print(
				ggplot(suspicious2, aes(x=reorder(AREA, CRITERIA), y=CRITERIA, 
				size=VARN)) + geom_point(aes(colour=POSITION)) + geom_hline(yintercept = seq(-2,2, by=4), colour="blue", linetype = "longdash" ) +
			     	labs(y="How far out of predicted value?", x=substring(area,1, ifelse(regexpr("_", area)[1]==-1, nchar(area), regexpr("_", area)[1]-1))) + 
				scale_y_continuous() + scale_colour_manual(values=tourism.cols(c(5,2))) + coord_flip() + 
			     	scale_size_area() + 
				ggtitle(paste(RTItype, " " ,substring(area,1, ifelse(regexpr("_", area)[1]==-1, nchar(area), regexpr("_", area)[1]-1)), 
				" where the latest ",  varn, " data \n does not fit in the prediction interval,\n",
				tmp$MONTH[length(tmp[,1])], " - ", tmp$YEAR[length(tmp[,1])], sep="")) +
				theme(plot.title=element_text(size=10, hjust=0.5))
			)
		dev.off()

		#  Tag countries listed in the top 32 NZ tourism markets according to the IVA year ended June 2012,
		if (area=="COUNTRY") {
			suspicious$IN_TOP32<-""
			suspicious$IN_TOP32<-ifelse(suspicious[,area] %in%
				c("Australia","Austria","Brazil","Canada","China","Cook Islands","Denmark","Fiji","France",
				"Germany","Hong Kong","India","Indonesia","Ireland","Italy","Japan","Korea, Republic of",
				"Malaysia","Netherlands","Philippines","Samoa","Singapore","South Africa","Spain","Sweden",
				"Switzerland","Taiwan, Province of China","Thailand","Tonga","United Arab Emirates","United Kingdom","United States"),"1","")
		}

		#Sort the data frame by descending order of varn
		suspicious.sort<-suspicious[order(suspicious[, varn], decreasing = TRUE), ]	
	}
	
	write.csv(suspicious.sort, paste(fpath, subf, RTItype,"  ",substring(area,1, ifelse(regexpr("_", area)[1]==-1, nchar(area), regexpr("_", area)[1]-1)), " ", varn,".csv", sep=""), row.names=FALSE)

}# END of autocheck function 
#######################################################################################################################

