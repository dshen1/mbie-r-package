\name{IVSvisits}
\alias{IVSvisits}
\docType{data}
\title{
International Visitor Survey - "visits"
}
\description{
visit summary (one line per leg of each respondent's trip ie one per "visit" to a region within New Zealand) from the International Visitor Survey (IVS) January 1997 to June 2013
}
\usage{data(IVSvisits)}
\format{
 The data has been imported from SPSS format and has XXX variables.  Full names of these variables is stored elsewhere in a data dictionary (TODO - where?).  They are mostly factor or numeric.
 }
\details{
%%  ~~ If necessary, more details than the __description__ above ~~
}
\source{
New Zealand Ministry of Business, Innovation and Employment
}
\references{
www.mbie.govt.nz
}

\seealso{
\code{\link{IVStrips}}, \code{\link{IVSspend}}
}

\examples{
library(plyr)

data(IVSvisits)

visits2012 <- subset(IVSvisits, YEDec==2012)

# create a list of requested marine activities
Marine <- c("Beaches (includes 90 Mile Beach)", "Scenic Boat Cruise", "Seal Colony",
            "Dolphin Watching/Swimming", "Penguins", "Whale Watching", "Kayaking Sea", 
            "Fishing Sea/Coastal", "Sailing")

# create a dataset that only has the variables we are interested in: ID vars, Weights,
# Activities
MoEv <- visits2012[, c(1, 205, 60:79)]

# melt into long form
MoEv.m <- melt(MoEv, id.vars=c("IDNumber", "FinalWeight"))
head(MoEv.m)

# subset data to include cases where marine activities are reported.
MoEv.sub <- subset(MoEv.m, value \%in\% Marine)
names(MoEv.sub) <- gsub("value", "Activity", names(MoEv.sub))
head(MoEv.sub)

#cast so that each person and activity has one line (no double counts), 
Marine.tab <- ddply(MoEv.sub, .(IDNumber, Activity), summarise, FinalWeight=max(FinalWeight))

# Add the number of unique individuals (weighted) per activity
Marine.tab2 <- ddply(Marine.tab, .(Activity), summarise, Tourists2012=round(sum(FinalWeight)))
Marine.tab2[order(-Marine.tab2$Tourists), ]


}
\keyword{datasets}
