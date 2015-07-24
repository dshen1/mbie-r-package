\name{autocheck}
\alias{autocheck}
\title{
Check for outliers in new monthly Regional Tourism Indicator data
}
\description{
A very specific function for analysing the latest month's Regional Tourism Indicators and detecting any outlier tourist origin, destination or industry that might warrant further investigation.
}
\usage{
autocheck(...)
}
\arguments{
  \item{x.df}{a data frame with transactions count and spend data}
\item{area}{a character string in quote which identify the column of Country names or destination RTO names or ANZSIC categories}
\item{varn}{???}
\item{RTItype}{a character string in quote to use as a prefix for folder names and output names to help to identify series "INTNL"=international or "DMSTC"=Domestic}
\item{fpath}{a character string in quote: folder path to locate where outputs are going to be saved.}


}

\details{
The function searches for outliers by modelling time series and forecasting the last month and then by comparing to the actual value. 

The function creates a subfolder if it doesn't exist and saves the png and csv files in it.  If there is any, look closer to the suspicious value(s)(=lower or greater to the confidence interval given by holtWinters.prediction()). If there is a least one potential outlier, the function creates also a graph to visualize the distance between the outlier and confidence interval given by the forecast.  The function creates a csv file with information about potential outlier(s); if there is none the csv file will have the message "No outlier detected.  For country series the function tag countries listed in the top 30 NZ tourism markets according to the IVA year ended June 2012.
}
\value{
NULL
}
\note{
This function is very specific to the MBIE environment and in particular a connection to the TRED1 database, and various interactive scripts used in the MBIE process for checking the RTIs each month.
}
\author{
Cherbane Sarri
}

\seealso{
\code{\link{creategraph}}

}
\examples{
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

subDirPath <- "P:/r/temp"  # or whatever being used as test environment

# should work
autocheck(cyspend, "Country", "Spend", "INTNL", subDirPath)
}
}

\keyword{ ts }

