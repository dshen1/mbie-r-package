\name{confplot}
\alias{confplot}

\title{
Dotchart with segments showing confidence intervals
}
\description{
Uses \code{confint()} to calculate confidence intervals from a single object created by \code{svyby()}, and then uses \code{dotchartG()} and \code{segments()} to draw a dotchart showing those confidence intervals.
}
\usage{
confplot(x, color = tourism.cols(1), ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
An object created by \code{svyby()}, with a \code{confint} method
}
  \item{color}{
Color for dots in the dotchart.  Defaults to the first colour from \code{tourism.cols()} which is "Forest" or pale green.
}
  \item{\dots}{
Other arguments to be passed to \code{dotchartG()} and hence to \code{dotchart()}.
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
NULL
}
\references{
%% ~put references to the literature/web site here ~
}
\author{
Peter Ellis
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\code{\link{svyby}},  \code{\link{dotchartG}}, \code{\link{compare}}, \code{\link{tourism.cols}}, \code{\link{segments}}
}
\examples{
}


\keyword{ htest }
\keyword{ survey }
