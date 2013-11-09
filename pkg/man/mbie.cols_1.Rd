\name{MBIE.cols}
\alias{MBIE.cols}
\docType{data}
\title{
MBIE colours - data
}
\description{
Hexadecimal coding for the seven colours in the MBIE style guide
}
\usage{data(MBIE.cols)}
\format{
  The format is:
 Named chr [1:7] "#006272" "#97D700" "#00B5E2" "#753BBD" "#DF1995" ...
 - attr(*, "names")= chr [1:7] "Teal" "Green" "Blue" "Purple" ...
}
\details{
It is not expected that you call this tiny dataset directly.  Instead, preferred way of accessing it is through the mbie.cols() function.
}
\source{

}
\references{
MBIE Style Guide (intranet link...)
}
\examples{
data(MBIE.cols)
MBIE.cols

mbie.cols("Teal")

mbie.cols("Trio3")

plotG(1:7, 1:7, col=mbie.cols(1:7), pch=19, cex=4)


}
\keyword{datasets}
