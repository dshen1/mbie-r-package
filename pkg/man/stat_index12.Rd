\name{stat_index12}
\alias{stat_index12}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Index on the fly to average of first twelve values
}
\description{
Converts groups of data to an index based on the mean of the first 12 values in each group.
}
\usage{
stat_index12(mapping = NULL, data = NULL, geom = "line", ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
\item{geom}{
Other than \code{line}, the most common geom to use is \code{point}.  They can be used together, as in the example below.
}

\item{\dots}{
Other arguments to get passed through to the geom eg \code{size=}, \code{shape=}.
}
}
\details{
Required aesthetics are x and y; x is usually a time variable.  
}
\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
\references{
%% ~put references to the literature/web site here ~
}
\author{
Peter Ellis <peter.ellis@mbie.govt.nz>
}
\note{
Data must be sorted so that when subsetted by group x is sequential from earliest to latest.  If \code{data} is created by group_by (dplyr) or ddply (plyr); gather (tidyr) or melt (reshape2); or spread (tidyr) or dcast (reshape2), this will usually happen naturally.  If \code{data} comes from a database you should incorporate a \code{ORDER BY ...} statement in the SQL that imports it to R.
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\code{\link{Index12}}, \code{\link{stat_index12}}, \code{\link{stat_sa}}, \code{\link{stat_rollapplyr}}
}
\examples{
\dontrun{
  library(RODBC)
  library(ggplot2)
  
  
  #Connect to TRED
  creds <- AskCreds()
  #TRED1 <- odbcConnect("TRED64", uid=creds$uid, pwd=creds$pwd)
  TRED1 <- odbcConnect("TRED_Prod")
  
  cyspend <- sqlQuery(TRED1, "select n.YEAR_NUMBER+(n.MONTH_NUMBER-.5)/12 period, 
    	YEAR_NUMBER Year, MONTH_NUMBER Month,  n.PERIOD_ID, CY_NAME Country,
  		SUM(SPEND_COUNT) Transactions, SUM(SPEND_AMOUNT) Spend
  		FROM RTI_MAIN.INTNL_SPEND s left join
  		RTI_MAIN.INTNL_DIMENSION d on 
  		s.DIMENSION_ID = d.DIMENSION_ID 
  		right join
  		(select  p.*,c.* from RTI_MAIN.PERIOD p cross join RTI_MAIN.COUNTRY c) n
  		on s.PERIOD_ID=n.PERIOD_ID and d.ORIGIN_ID= n.CY_N4_CODE
  		group by YEAR_NUMBER, MONTH_NUMBER, n.PERIOD_ID, CY_NAME
  		order by YEAR_NUMBER, MONTH_NUMBER, CY_NAME")
  
  # subset to five countries (nb could also be done more efficiently in the SQL query)
  cyspend_small <- subset(cyspend, 
      Country \%in\% c("Australia", "Germany", "France", "Japan", "India"))
  
  # Plot with indexing
  ggplot(cyspend_small, aes(x=period, y=Spend, color=Country)) +
    stat_index12()
    
  # plot with indexing, and seasonally adjusted line superimposed
  ggplot(cyspend_small, aes(x=period, y=Spend, color=Country)) +
    stat_index12(alpha = 0.5) +
    stat_sa(index12.first = TRUE)
    }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~ts }
\keyword{ ~dplot }
