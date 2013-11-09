\name{DTSresp}
\alias{DTSresp}
\docType{data}
\title{
International Visitor Survey - "trips"
}
\description{
Trip summary (one line per respondent household) from the Domestic Travel Survey (IVS) January 1997 to June 2012
}
\usage{data(DTSresp)}
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
\code{\link{IVSvisits}}, \code{\link{IVSspend}}
}

\examples{
library(plyr)
library(scales)
library(ggplot2)

data(IVStrips)
tmp <- ddply(IVStrips, .(YearQuarter, POVTop3, COPRTop5), summarise, Total= sum(SmoothTotalSpend * FinalWeight))
tmp$period <- with(tmp, as.numeric(substring(YearQuarter, 1, 4)) + (as.numeric(substring(YearQuarter, 6, 6))-.5)/4)

ggplot(tmp, aes(x=period, y=Total, color=POVTop3)) +
  stat_sa(frequency=4, geom="point", size=2) +
  stat_sa(frequency=4, size=2) +
  geom_line(frequency=4, size=1, alpha=.5) +
  scale_color_manual(values=tourism.cols("Alternating")) +
  facet_wrap(~COPRTop5, scale="free_y") +
  ggtitle("Seasonally adjusted spend by purpose of visit")



}
\keyword{datasets}
