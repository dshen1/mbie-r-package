\name{Index12}
\alias{Index12}

\title{
Convert a vector into an index with reference point the average of the first 12 values
}
\description{
Function that takes an object and converts it to an index, with the reference value that is set to 100 defined as the average of the first 12 values.  
}
\usage{
Index12(x, ref = mean(x[1:12]), stripNAs=FALSE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
Object (usually a vector or time series) to be converted into an index.
}
  \item{ref}{
Reference value at which the index will be set to be 100; defaults to mean of the first 12 elements of x.
}
	\item{stripNAs}{
Logical; should NAs be stripped from the vector being indexed first? 	
	}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
An object of the same dimension and class as x but with values transformed to an index.}
\references{
%% ~put references to the literature/web site here ~
}
\author{
Peter Ellis <peter.ellis@mbie.govt.nz>
}
\note{
Exactly the same as Index() except for the default value of ref=.
}



\seealso{
\code{\link{Index}},
}
\examples{
data(AirPassengers)
plot(cbind(AirPassengers, Index12(AirPassengers)))
}

\keyword{ ts }

