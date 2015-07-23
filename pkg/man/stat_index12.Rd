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
  library(dplyr)
  
  
  #Connect to TRED
   TRED <- odbcConnect("TRED_Prod")

   cyspend <- 
      sqlQuery(TRED, "
         SELECT 
            period, Country, SUM(SpendCount) AS Transactions, SUM(SpendAmount) AS Spend
         FROM
            (SELECT 
               b.Year + (b.MonthNumber - 0.5)/12 AS Period, 
               Origin Country,
               SpendCount,
               SpendAmount
            FROM [Production].[RTISpendPivot] a
               LEFT JOIN [Production].[RTISurveyMainHeader] b
               ON a.[SurveyResponseID] = b.[SurveyResponseID]
               WHERE RecordType = 'International' 
            ) c
            GROUP BY Period, Country
         ")
         
      head(cyspend, 10)
      

   # subset to five countries (nb could also be done more efficiently in the SQL query)
      cyspend_small <- subset(cyspend, 
         Country \%in\% c("Australia", "Germany", "France", "Japan", "India"))
  
   # Plot with indexing
      ggplot(cyspend_small, aes(x=period, y=Spend, color = Country)) +
         stat_index12()
    
   # plot with indexing, and seasonally adjusted line superimposed
      ggplot(cyspend_small, aes(x = period, y = Spend, color = Country)) +
      stat_index12(alpha = 0.2, size = 1) +
      stat_sa(index12.first = TRUE)
   }
}

% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~ts }
\keyword{ ~dplot }
