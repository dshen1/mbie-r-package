\name{mbie-package}
\alias{mbie-package}
\alias{mbie}
\docType{package}
\title{

Miscellaneous functions and data for the New Zealand Ministry of Business, Innovation and Employment
}
\description{
A collection of functions, colour palettes and data commonly used by the New Zealand Ministry of Business, Innovation and Employment. 
}
\details{
\tabular{ll}{
Package: \tab mbie\cr
Type: \tab Package\cr
Version: \tab 0.6.0\cr
Date: \tab 2013-11-14\cr
License: \tab GPL-3\cr
}
This package provides:

\emph{Graphics functions} such as:
\enumerate{
    \item{Convenience functions to make base graphics have a better default look and feel for the MBIE Sector Performance style, or to do back of the envelope calculations or comparisons from the analysis of surveys - like \code{plotG()}, \code{dotchartG()}, and \code{compare()}}
    \item{implementations of plots for which there are not standard, easy to access implementations - like \code{qqNormEnv()} and \code{Wordcloud()} (the latter a convenient wrapper to functions written by non-MBIE authors)}
    \item{enhancements to \code{ggplot} by adding statistical transformations on the fly like \code{stat_sa()} and \code{stat_index12()}}; and helping layout via the widespread function \code{vplayout}.
}

Functions for \emph{common, basic tasks} such as transforming a series to an index via \code{Index()} or \code{Index12}, converting a vector of detailed country names into a factor with 13 country groupings, or calculating a compound annual growth rate through \code{CAGR()}.


Functions giving convenient access to \emph{palettes of colours} including \code{mbie.cols()} for the bright colours prescribed in the MBIE style guide, and \code{tourism.cols()} for the colours commissioned by the Ministry of Tourism in 2010 and still used by MBIE for tourism statistical graphics.

\emph{Data management or import functions}, the most important of which are \code{rename.levels()} and \code{xlsToR()} (the latter ported completely from another author on the web)

The following \emph{rectangular datasets}, accessible by \code{data(...)} and with help available from ?.  These are datasets that are either finished collection (RVM, the old IVS methodology files) or only infrequently updated (RTEs, forecasts)
\enumerate{
  \item{IVStrips} - International Visitor Survey, "old method" 1997-2013
  \item{IVSspend}
  \item{IVSvisits}
  \item{RTEs} - Regional Tourism Estimates
  \item{forecasts} - New Zealand tourism forecasts
  \item{DTSresp} - Domestic Travel Survey
  \item{DTStrips}
  \item{DTSvisits}
  \item(RVM) - Regional Visitor Monitor
  \item(locations) - reference table of locations used in the DTS and the old method IVS
  \item{TA_lookup} - reference table showing which "RTI TA" (modified Territorial Authority as used in production of the Regional Tourism Indicators) is in which genuine TA as used by Statistics New Zealand.
}
  
The following \emph{spatial polygon dataframes}, also accessible by \code{data(...)} and with help available from ?.
\enumerate{
  \item{RTO} - boundaries of Regional Tourism Organisations (with some minor quibbles in Canterbury)
  \item{TA_TRED} - boundaries of modified Territorial Authorities (called TA_TRED for historical reasons) - similar to Territorial Authorities as defined by Statistics New Zealand, but with a few subdivisions.  For example, Southland is split into Fiordland and Southland; Auckland is split into six territories.
  \item{Region} - Regional Council boundaries in 2012, imported from a Statistics New Zealand shape file and projected to latitude and longitude
  \item{TA} - Territorial Authority boundaries in 2012, imported from a Statistics New Zealand shape file and projected to latitude and longitude
  \item{AU} - Area Unit boundaries in 2012, imported from a Statistics New Zealand shape file and projected to latitude and longitude
}

The following raster image, created by importing a portable networks graphics file:
\enumerate{
  \item{logo}
}

The following postscript images, created by importing via grImport:
\enumerate{
  \item{logo_white_ps}
  \item{logo_black_ps}
}

}
\author{
MBIE Sector Performance team

Maintainer: Peter Ellis <peter.ellis@mbie.govt.nz>

}
\references{
www.mbie.govt.nz
}
%%~~ Optionally other standard keywords, one per line, from file KEYWORDS in the R ~~
%%~~ documentation directory ~~
\keyword{ package }
\seealso{

\code{\link[<pkg>:<pkg>-package]{ggplot2}}, \code{\link[<pkg>:<pkg>-package]{wordcloud}}, \code{\link[<pkg>:<pkg>-package]{XML}}, \code{\link[<pkg>:<pkg>-package]{tm}}, \code{\link[<pkg>:<pkg>-package]{grid}}, \code{\link[<pkg>:<pkg>-package]{survey}}, \code{\link[<pkg>:<pkg>-package]{zoo}}
}
\examples{


}
