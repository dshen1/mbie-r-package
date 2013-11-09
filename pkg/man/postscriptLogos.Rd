\name{postscriptLogos}
\alias{logo_ps}
\alias{logo_white_ps}
\alias{logo_black_ps}
\docType{data}
\title{
Encapsulated postscript versions of the MBIE logo
}
\description{
\code{logo_white_ps} and \code{logo_black_ps} are vector format versions of the MBIE logo, appropriate for use (high quality) when writing graphics to a vector format graphics device like pdf or svg.
}
\usage{data(logo_black_ps); data(logo_white_ps)}
\format{
 "Picture" objects imported via RGML under the \code{grImport} package
 }
\details{

}
\source{
New Zealand Ministry of Business, Innovation and Employment
}
\references{
www.mbie.govt.nz
}

\note{
Doesn't necessarily look that good on screen but should be good when saved to a pdf.  If you are printing a plot to screen or to a raster device (eg png) it is better to use \code{logo}, which is based on an imported png image.
}

\seealso{
\code{\link{logo}}
}

\examples{
require(grImport)
data(logo_black_ps)
plot(1:10, 1:10)
grid.picture(logo_black_ps, 0.8, 0.8, width=0.3)

}
\keyword{datasets}
