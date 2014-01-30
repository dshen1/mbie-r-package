\name{Index}
\alias{Index}

\title{
Convert a vector into an index
}
\description{
Function that takes an object and returns it as an index based on 100.
}
\usage{
Index(x, ref = x[1], stripNAs=FALSE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
Object (usually a vector or time series) to be converted into an index.
}
  \item{ref}{
Reference value at which the index will be set to be 100; defaults to the first element of x.
}
\item{stripNAs}{
Logical; should NAs be stripped from the vector being indexed first? 	
	}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
An object of the same dimension and class as x but with values transformed to an index.
}
\references{
%% ~put references to the literature/web site here ~
}
\author{
Peter Ellis <peter.ellis@mbie.govt.nz>
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\code{\link{Index12}}, ~~~
}
\examples{
data(AirPassengers)
plot(cbind(AirPassengers, Index(AirPassengers)))

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ts }

