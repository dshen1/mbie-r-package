\name{mbie_footer}
\alias{mbie_footer}
\alias{MBIE_footer}
\alias{footer}
\title{
MBIE footer 
}
\description{
MBIE footer is imported from a PNG format file by getPNG from the png package. 
}
\usage{
data(mbie_footer)
}
\format{
Raster image
}
 \arguments{
  NULL
}

\source{
New Zealand Ministry of Business, Innovation and Employment
}
\references{
www.mbie.govt.nz
}

\note{
This MBIE footer is included in the mbie package for the sake of transparency and simplicity (so the package can be used by MBIE staff as by re-users of our data)
We might change this at some point in the future.  
Please note that the Creative Commons licence that governs the mbie package as a whole does not apply to any logos, emblems and trade marks or to website or email design elements or to any photography. 
Those specific items may not be re-used without express permission.  
}

\seealso{
data(logo)
}
\examples{
data(mbie_footer)
plot(1:10, 1:10)
grid.raster(mbie_footer, 0.5, 0.03, width=1)
}

\keyword{datasets}

?mbie\author{
Antonia Milkop, September 2014
}
