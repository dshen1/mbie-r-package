#-------Tourism palette------------------------

# Create a vector of the Ministry of Tourism colours
Tourism.cols <- c(
   Forest = "#3D4721FF",
   Grass = "#A8B50AFF",
   Southerly="#5C788FFF",
   Volcano="#B09E0DFF",
   Koura="#D4470FFF",
   Merino="#C2C4A3FF",
   Moss= "#5E7803FF",
   Pohutukawa="#AD2624FF",
   Sunrise="#D48500FF",
   CityLights ="#EDBD3DFF",
   SouthernCross="#265787FF",
   WineCountry="#61384DFF",
   Flax="#708270FF",
   Ocean="#A8C4C4FF",
   RiverStone="#ADABA6FF",
   Waka="#826E59FF",
   CabbageTree="#A8AD70FF",
   Sky="#94B5E0FF")

# A function tourism.cols() for easy reference to the vector Tourism.cols 
tourism.cols <- function(x=c(1,2,3,5,12,6)){
   # function to return in vector format a subset of the Ministry of Tourism's 2007
   # palette of colors.  By default returns 6 colours that form a nice set
   # for most plots.  Note that normally the 4th colour (Volcano) is too similar
   # to the 2nd (Grass) for use in plots.
   if(x[1]=="Primary") x<-1:6
   if(x[1]=="Supporting") x<-7:18
   if(x[1]=="All") x <-1:18
   if(x[1]=="Pale")x <- 13:18
   if(x[1]=="Alternating") x <- rep(c(1,6,4,8,2,16,3,5,15,18,9,14,7,12,11,17,10,13),6)
   as.vector(Tourism.cols[x])
}



#------------------MBIE palette-------------------------

# Create a vector to store the colours
MBIE.cols <- c(
   Teal=rgb(0,98,114, maxColorValue=255),
   Green=rgb(151,215,0, maxColorValue=255),
   Blue=rgb(0,181,226, maxColorValue=255),
   Purple=rgb(117,59,189, maxColorValue=255),
   Pink=rgb(223,25,149, maxColorValue=255),
   Orange=rgb(255,105,0, maxColorValue=255),
   Yellow=rgb(251,225,34, maxColorValue=255)
)

# Create a function for easy reference to combinations of MBIE.cols
mbie.cols <- function(x=1:7){
   
   # function to return in vector format a subset of the MBIE 2013
   # palette of colors. Use in the form mbie.cols("Trio1") will give
   # one of the approved combinations of 2 or 3 colors
   
   if(x[1]=="Duo1") x<- 1:2
   if(x[1]=="Trio1") x<- 1:3
   if(x[1]=="Duo2") x <- 2:3
   if(x[1]=="Trio2")x <- 3:5
   if(x[1]=="Duo3")x <- 4:5
   if(x[1]=="Trio3") x <- c(4,6:7)
   if(x[1]=="Duo4") x <- 6:7
   if(x[1]=="Duo5") x <- c(4,7)
   
   as.vector(MBIE.cols[x])
}


#-----------qqNormEnv--------------------

qqNormEnv <- function(samp, ribbon.fill=alpha("steelblue", 0.1),...){
   # A ggplot2 function to draw a QQNorm plot with an envelope
   # of simulated values that would show where the data would be
   # if it really were normally distributed
   n <- length(samp)
   tmp <- scale(cbind(samp, matrix(rnorm(n*19), nrow=n)))
   tmp <- apply(tmp,2, sort)
   R <- t(apply(tmp[,-1],1,range))
   xsamp <- qqnorm(tmp[,1], plot=F)$x
   return(qplot(sample=tmp[,1], stat="qq",...) +
             geom_ribbon(aes(ymin=R[,1], ymax=R[,2], x=xsamp), fill=ribbon.fill))
}


#-------vplayout-----------------


vplayout <- function(x,y) {
   # Function to help in regular layouts in grid environment
   # this function taken from Hadley Wickham's ggplot2 book
   viewport(layout.pos.row=x, layout.pos.col=y)
}

#----------Wordcloud--------------

Wordcloud <- function(directory, colors=tourism.cols(12:7), ...){
   # a simple wrapper function that draws a wordcloud
   # using the text file/s at location directory.
   # x needs to be a location of a folder on the network drive eg
   # "P:/OTSP/textfiles/BIM"
   # - and that folder needs to have at least one text file in it.
   
   txt <- Corpus(DirSource(directory))
   ap.corpus <- tm_map(txt, removePunctuation)
   ap.corpus <- tm_map(ap.corpus, content_transformer(tolower))
   ap.corpus <- tm_map(ap.corpus, function(x) removeWords(x, stopwords("english")))
   ap.tdm <- TermDocumentMatrix(ap.corpus)
   ap.m <- as.matrix(ap.tdm)
   ap.v <- sort(rowSums(ap.m), decreasing = TRUE)
   ap.d <- data.frame(word = names(ap.v), freq = ap.v)
   print(wordcloud(ap.d$word, ap.d$freq, random.order = FALSE, colors = colors, ...))
   
}



#--------Greying base graphics--------------------------------

# Functions for making base graphics plots look a little more like ggplot2
# plots, with grey backgrounds and solid plotting characters


plotG <- function(..., col=tourism.cols(2), pch=19, bty="l", Fill="grey95", xAdj=1000, yAdj=1000){
   # a function for drawing a points or line chart on a grey background
   # with a white grid	
   plot(..., bty=bty)
   
   X <-par()$usr
   # xAdj and yAdj sometimes need to be tweaked to make the rectangle appear in the right spot.  Try 100.
   xlen <-(X[2]-X[1])/xAdj
   ylen <-(X[4]-X[3])/yAdj
   rect(X[1]+xlen, X[3]+ylen, X[2]-xlen, X[4]-ylen,col=Fill, border=Fill)
   grid(col="white", lwd=2, lty=1)
   
   points(..., col=col, pch=pch)
}

dotchartG <- function(x, color=tourism.cols(1), ...){
   # a function for drawing a dotchart on a grey background
   # with white horizontal lines
   warning("dotchartG is deprecated and does not work with more than one dimension, use ggplot with facet_wrap instead")
   dotchart(x, color=color, ...)
   rect(par("usr")[1],par("usr")[3],par("usr")[2],par("usr")[4],col = "gray95")
   grid(col="white", lty=1)
   rect(par("usr")[1],par("usr")[3],par("usr")[2],par("usr")[4],border = "gray95")
   points(x, 1:length(x), pch=19, col=color)
}



#----------------survey package wrapper plots--------------------------

# Functions for making the output of survey package commands a little
# more user friendly.  Not - relies on dotchartG, defined earlier

confplot <- function(x, color=tourism.cols(1),...){
   # a function for drawing 95% confidence interval plots
   # from a single object created by svyby
   dotchartG(x[,2],	xlim=range(confint(x)), labels=as.character(x[,1]),...)
   segments(confint(x)[,1], 1:nrow(x), confint(x)[,2], 1:nrow(x), col=color)
}




compare <- function(a, b, color=tourism.cols(1),...){
   # a function for comparing the difference between two independent
   # results from svyby, when svyby is "by" just one variable
   # Draws a plot, and returns a confidence interval for the difference
   x <- a[,2] - b[,2] # difference
   dif <-sqrt(a[,3]^2 + b[,3]^2) # s.e. of difference
   res <- data.frame(est=x, lower=x -1.96  * dif, upper=x+1.96*dif)
   row.names(res) <- row.names(a)
   dotchartG(res[,1], labels=row.names(res), xlim=range(res),...)
   segments(res[,2], 1:nrow(res), res[,3], 1:nrow(res), col=color)
   abline(v=0)
   return(res)
}

#----------------------------Map of world with NZ in centre-----------------------------------


plot_map<- function(database,center,...){ 
   #http://stackoverflow.com/questions/5353184/fixing-maps-library-data-for-pacific-centred-0-360-longitude-display
   require(maps)
   Obj <- map(database,...,plot=F) 
   coord <- cbind(Obj[[1]],Obj[[2]]) 
   
   # split up the coordinates 
   id <- rle(!is.na(coord[,1])) 
   id <- matrix(c(1,cumsum(id$lengths)),ncol=2,byrow=T) 
   polygons <- apply(id,1,function(i){coord[i[1]:i[2],]}) 
   
   # split up polygons that differ too much 
   polygons <- lapply(polygons,function(x){ 
      x[,1] <- x[,1] + center 
      x[,1] <- ifelse(x[,1]>180,x[,1]-360,x[,1]) 
      if(sum(diff(x[,1])>300,na.rm=T) >0){ 
         id <- x[,1] < 0 
         x <- rbind(x[id,],c(NA,NA),x[!id,]) 
      } 
      x 
   }) 
   # reconstruct the object 
   polygons <- do.call(rbind,polygons) 
   Obj[[1]] <- polygons[,1] 
   Obj[[2]] <- polygons[,2] 
   
   map(Obj,...) 
} 





#----------------rename.levels----------------------------------

rename.levels <- function(x, orig, new){
   # Function for relabelling some of the levels of a factor.
   # Useful eg for changing UK to United Kingdom, etc
   #
   # x is a factor with some levels to be relabelled
   # orig is the levels of x (not all levels needed) that need to be relabelled
   # new is the new labels - must exactly match orig in length
   #
   if (!is.factor(x)) stop("x must be a factor")
   if (length(orig) != length(new)) stop("Number of new labels must equal number of old labels.")
   for (i in 1:length(orig)){
      levels(x)[levels(x)==orig[i]] <- new[i]
   }
   return(x)
}



#----------------confidentialising-----------
# Next function is used for confidentialising output from Statistics New Zealand unit record data

Roundthis <- function(x, type="Graduated"){
   # This function rounds each value in x depending on the size of the value
   # Performs random rounding using the Statistics New Zealand RRing policy.
   # Author - Peter Ellis 26 June 2013
   # Adapted from an Excel macro created January 2002 by Victoria Wilcox and Vicky Barlow,
   #   which was modified by James Enright and by Paul Cowie in 2002 and 2008.
   #
   # Will work with either a vector or a matrix.
   #
   # Modified PE 27 June to vectorise the code that creates remainder, much faster
   # Modified PE 24 August to fix a bug and further vectorise code.
   
   temp <- abs(x)
   
   if(type=="Graduated"){
      RRB <- ifelse(temp<=18, 3,
                    ifelse(temp<20,2,
                           ifelse(temp<100,5,
                                  ifelse(temp<1000,10,100)
                           )
                    )
      )
   } else if (type=="Base3"){
      RRB <- rep(3, length(temp))
   } else stop("Type must be Graduated or Base3")
   
   # Create vector of "remainders"
   remainder <- (temp/RRB-trunc(temp/RRB))*RRB
   
   # This next bit vectorised by PE 24 August 2013 - faster
   # (and also removed a bug that impacted when remainder was zero).
   Numlow_all <- temp-remainder
   
   x2 <- sign(x) * ifelse(remainder!=0, 
                          ifelse(runif(length(temp)) * RRB <= remainder,
                                 Numlow_all + RRB, Numlow_all),
                          temp)
   
   return(x2)  
}





#------------CAGR-----------

CAGR <- function(ratio, period, digits = 1){
   # calculate the average annual growth rate needed for something to grow
   # from 1 to the value of ?ratio? over ?period? years.? Returns a percentage.
   round((exp(log(ratio)/period) - 1) * 100, digits)
}



#------------Importance - dodgy market research calcs---------------------------------------
Importance <- function(x, target=x[ , ncol(x)], use="pairwise.complete.obs", Scale=FALSE, Plot=FALSE, Round=TRUE, ...){
   # Function returns the R-squareds for each column in x predicting the target.
   # If no target is given, uses the final column of x.
   # Scale is recommended to be FALSE.  If true, it scales all the R-squared so they add to 100.
   # That is what TNS used to do for the VEM to derive "importance" scores but it loses valuable
   # information and makes the resulting scores somewhat misleading.  Capacity has been built in
   # for checking purposes.
   
   # Check target is correct length
   if (length(target) != nrow(x)){
      stop("Target must be a vector of same length as number of rows in x.")
   }
   
   # Create a single matrix (if we haven't got one already)
   if (sum(target != x[ , ncol(x)], na.rm=TRUE) != 0){
      x <- cbind(x, target)
   }
   
   # Draw pairs plot of correlations, if asked
   if (Plot) {
      require(GGally)
      print(ggpairs(jitter(as.matrix(x[complete.cases(x), ])), ...))
   }
   
   # Estimate R-squareds and convert to "percentage"
   p <- ncol(x) 
   R2 <- cor(x, use=use)[p, -p] ^ 2 * 100
   
   # Scale so they all add to 100, if asked
   if (Scale){
      R2 <- R2 * 100 / sum(R2)
   }
   
   # Round to nearest percentage, if asked
   if (Round){
      R2 <- round(R2, ...)
   }
   
   # Return the result - not that it is a percentage, not a proportion as the original R-squared was
   return(R2)
}

