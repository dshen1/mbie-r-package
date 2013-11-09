\name{creategraph}
\alias{creategraph}
\title{
Draw many time series plots as part of monthly Regional Tourism Indicators checks
}
\description{
Draw graphs for spend and transactions for each of a subset of Regional Tourism Indicators (RTI) data
}
\usage{
creategraph(...)
}


 
\arguments{
  \item{x.df}{a data frame with transactions count and spend data}
  \item{area}{a character string in quote which identify the column of Country names or destination RTO names or ANZSIC categories}
  \item{RTItype}{a character string in quote to use as a prefix for folder names and output names to help to identify series "INTNL"=international or "DMSTC"=Domestic}
    \item{fpath}{a character string in quote: folder path to locate where outputs are going to be saved}
}
\details{
The function creategraph creates 2 graphs (top and bottom of the device, to give a good time series aspect ratio) in a png file for each  levels or values of a vector of character strings (it could be country names, RTO names or ANZSIC categories). The function creates a subfolder if it doesn't exist and saves the png files in it.

\code{creategraph()} is not usually called directly by the user, but is repeatedly called by \code{autocheck.Rd()}.
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
\code{\link{autocheck}}

}
\examples{
  
}

\keyword{ ts }


