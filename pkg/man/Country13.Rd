\name{Country13}
\alias{Country13}

\title{
Convert "detailed" countries into a grouping of 13 
}
\description{
Function that takes a factor or character vector and returns a factor with 13 levels, matching the country groupings in the IVS as used from July 2013.
}
\usage{
Country13(x, shorten=TRUE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
Vector with class character or factor, with levels or values indicating countries
}
 \item{shorten}{
Logical - should the long names for China, USA and UK be abbreviated?
} 
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
A factor of the same length as x but with 13 levels.
}
\references{
%% ~put references to the literature/web site here ~
}
\author{
Peter Ellis <peter.ellis@mbie.govt.nz>
}
\note{

}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\code{\link{IVStrips}}, ~~~
}
\examples{
data(IVStrips)
table(Country13(IVStrips$COPRDetail))
table(Country13(IVStrips$COPRDetail, shorten=FALSE))

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ manip }

