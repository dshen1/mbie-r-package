\name{logo}
\alias{logo}
\docType{data}
\title{
MBIE logo
}
\description{
MBIE logo, imported from a PNG format file by getPNG from the png package
}
\usage{data(logo)}
\format{
 Raster image
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
data(logo)
plot(1:10, 1:10)
grid.raster(logo, 0.5, 0.9, width=0.3)


}
\keyword{datasets}
