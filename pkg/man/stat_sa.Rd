\name{stat_sa}
\alias{stat_sa}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Seasonal adjustment on the fly
}
\description{
Performs classical seasonal decomposition on the fly as a ggplot2 statistical transformation.  This allows quick and easy production of seasonally adjusted graphics even for groupings of data.
}
\usage{
stat_sa(mapping = NULL, data= NULL, geom = "line", frequency = 12, type = "multiplicative", index12.first = FALSE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
 \item{geom}{
  Other than \code{line}, the most common geom to use is \code{point}.  They can be used together, as in the example below.
  }
  \item{frequency}{Frequency of regular period for which seasonal adjustment is controlling - defaults to 12 ie for monthly data}
  \item{index12.first}{logical; should the series first be transformed so it is an index, with the mean of the first 12 values set to be 100}
  \item{type}{"multiplicative" or "additive" - passed through to decompose for which type of seasonality is assumed}
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
Data must be sorted so that when subsetted by group x is sequential from earliest to latest.  If \code{data} is created by ddply, melt or dcast this will usually happen naturally.  If \code{data} comes from a database you should incorporate a \code{ORDER BY ...} statement in the SQL that imports it to R.
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\code{\link{decompose}}, \code{\link{stat_index12}}, \code{\link{stat_index1}}, \code{\link{stat_rollapplyr}}
}
\examples{
#----------------------IVS example with quarterly data-----------------------
library(dplyr)
library(scales)
library(ggplot2)
library(mbiedata)
data(IVStrips)

tmp <- IVStrips \%>\%
            group_by(YearQuarter, POVTop3, COPRTop5) \%>\%
            summarise(Total = sum(SmoothTotalSpend * FinalWeight * Adjusted_Factor))

tmp$period <- with(tmp, as.numeric(substring(YearQuarter, 1, 4)) + 
                    (as.numeric(substring(YearQuarter, 6, 6))-.5)/4)

ggplot(tmp, aes(x = period, y = Total, color = POVTop3)) +
  stat_sa(frequency = 4, geom = "point", size = 1) +
  stat_sa(frequency = 4, size = 1) +
  geom_line(frequency = 4, size = 0.5, alpha = 0.5) +
  scale_color_manual(values = tourism.cols("Alternating")) +
  facet_wrap(~COPRTop5, scale = "free_y") +
  ggtitle("Seasonally adjusted spend by purpose of visit")
  

#-------------RTIs example, incorporating indexing------------------------  
\dontrun{
  library(RODBC)
  library(ggplot2)
  
  
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
  
  
  # subset to five countries (nb could also be done more efficiently in the SQL query)
  cyspend_small <- subset(cyspend, 
      Country \%in\% c("Australia", "Germany", "France", "Japan", "India"))
  
  # Plot with indexing
  ggplot(cyspend_small, aes(x=period, y=Spend, color=Country)) +
    stat_index12()
    
  # plot with indexing, and seasonally adjusted line superimposed
  ggplot(cyspend_small, aes(x=period, y=Spend, color=Country)) +
    stat_index12(alpha = 0.2, size = 1) +
    stat_sa(index12.first = TRUE)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~ts }
\keyword{ ~dplot }
