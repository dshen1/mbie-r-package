\name{stat_index1}
\alias{stat_index1}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Index on the fly
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
stat_index1(mapping = NULL, data = NULL, geom = "line", ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
\item{geom}{
Other than \code{line}, the most common geom to use is \code{point}.  They can be used together, as in the example below.
}

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
\code{\link{Index}}, \code{\link{stat_index12}}, \code{\link{stat_sa}}, \code{\link{stat_rollapplyr}}
}
\examples{
require(dplyr)
require(ggplot2)
require(mbiedata)
data(IVStrips)

tourists <- IVStrips \%>\%
               filter(YEJun > 1997) \%>\%
               group_by(YEJun, COPRTop5) \%>\%
               summarise(Spend = sum(SpendxWeight * Adjusted_Factor))

ggplot(tourists, aes(x = YEJun, y = Spend, colour = COPRTop5)) +
  stat_index1() +
  stat_index1(geom = "point", shape = 2, size = 3)


}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~ts }
\keyword{ ~dplot }
