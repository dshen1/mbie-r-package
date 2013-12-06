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

\note{
This logo is included in the mbie package for the sake of transparency and simplicity (so the same package can be used by MBIE staff as by re-users of our data).  We might change this at some point in the future.  Please note that the Creative Commons licence that governs the mbie package as a whole does not apply to any logos, emblems and trade marks or to website or email design elements or to any photography. Those specific items may not be re-used without express permission.  
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
