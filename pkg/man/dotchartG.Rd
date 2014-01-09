\name{dotchartG}
\alias{dotchartG}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Draw a dotchart with solid dots and grey background
}
\description{
A simple wrapper around dotchart() that means it appears with solid dots, a grey background and white gridlines instead of the usual defaults.
}
\usage{
dotchartG(x, color = tourism.cols(1), ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
An object with which to draw a dotchart.  See help file for dotchart.
}
  \item{color}{
Colour to use for the dots.  Defaults to "Forest" or pale green.
}
  \item{\dots}{
Other arguments to be passed to dotchart.
}
}
\value{
NULL
}
\references{
%% ~put references to the literature/web site here ~
}
\author{
Peter Ellis <peter.ellis@mbie.govt.nz>
}
\note{
Deprecated, and only kept in so some legacy code does not need to be re-written.  Use ggplot in combination with facet_wrap instead.
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\code{\link{plotG}}, \code{\link{dotchart}}
}
\examples{


}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ aplot }

