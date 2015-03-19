\name{stat_rollapplyr}
\alias{stat_rollapplyr}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Rolling average on the fly
}
\description{
Calculates and draws a rolling average line (or points) for each subset of data in a ggplot graphic.
}
\usage{
stat_rollapplyr(mapping = NULL, data = NULL, geom = "line", width = 12, fill = NA, align = "right", index12.first = FALSE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
 \item{geom}{
  Other than \code{line}, the most common geom to use is \code{point}.  They can be used together, as in the example below.
  }
  \item{width}{Width of period for which rolling mean is calculated - defaults to 12}
  \item{index12.first}{logical; should the series first be transformed so it is an index, with the mean of the first 12 values set to be 100}
  \item{fill, align}{arguments passed through to zoo::rollapplyr}
  \item{\dots}{
  Other arguments to get passed through to the geom eg \code{size=}, \code{shape=}.
  }

}
\details{
Required aesthetics are x and y; x is usually a time variable.
}
\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
\references{
%% ~put references to the literature/web site here ~
}
\author{
Peter Ellis <peter.ellis@mbie.govt.nz>
}
\note{
Data must be sorted so that when subsetted by group x is sequential from earliest to latest.  If \code{data} is created by ddply, melt or dcast this will usually happen naturally.  If \code{data} comes from a database you should incorporate a \code{ORDER BY ...} statement in the SQL that imports it to R.
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\code{\link{zoo:rollapplyr}}, \code{\link{stat_index12}}, \code{\link{stat_index1}}, \code{\link{stat_rollapplyr}}
}
\examples{
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

## The function is currently defined as
#  structure(function (...) 
#  get(x, envir = this, inherits = inh)(this, ...), class = c("instantiatedProtoMethod", 
#  "function"), this = <environment>)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~ts }
\keyword{ ~dplot }
