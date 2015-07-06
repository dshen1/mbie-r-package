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
Version: \tab 0.9.0\cr
Date: \tab 2015-07-06\cr
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

The following reference \emph{rectangular datasets}, accessible by \code{data(...)} and with help available from ?.  
\enumerate{
   \item(locations) - reference table of locations used in the DTS and the old method IVS
  \item{TA_lookup} - reference table showing which "RTI TA" (modified Territorial Authority as used in production of the Regional Tourism Indicators) is in which genuine TA as used by Statistics New Zealand.
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

Maintainer: Pete McMillen <pete.mcmillen@mbie.govt.nz>

}
\references{
www.mbie.govt.nz
}
%%~~ Optionally other standard keywords, one per line, from file KEYWORDS in the R ~~
%%~~ documentation directory ~~



\note{
Crown Copyright. This data and software is protected by copyright. Unless indicated otherwise (either below or within specific items or collections), this copyright material is licensed for re-use under the Creative Commons Attribution 3.0 New Zealand licence. In essence, you are free to copy, distribute and adapt the material, as long as you attribute it to the Ministry of Business, Innovation and Employment and abide by the other licence terms. Please note that this licence does not apply to any logos, emblems and trade marks or to website or email design elements or to any photography. Those specific items may not be re-used without express permission.
}


\keyword{ package }
\seealso{
\code{\link[<pkg>:<pkg>-package]{mbiedata}}, 
\code{\link[<pkg>:<pkg>-package]{mbiemaps}}, \code{\link[<pkg>:<pkg>-package]{ggplot2}}, \code{\link[<pkg>:<pkg>-package]{wordcloud}}, \code{\link[<pkg>:<pkg>-package]{XML}}, \code{\link[<pkg>:<pkg>-package]{tm}}, \code{\link[<pkg>:<pkg>-package]{grid}}, \code{\link[<pkg>:<pkg>-package]{survey}}, \code{\link[<pkg>:<pkg>-package]{zoo}}
}
\examples{


}
