\name{MemoryObjUse}
\alias{MemoryObjUse}
\concept{MemoryUse}
\concept{MemoryObjectUse}
\concept{ObjectMemoryUse}
\concept{ObjMemoryUse}

\title{
Size of memory used by objects - function
}
\description{
Function to return a list of objects currently being used by R, and their associated memory use (Kb)
}
\usage{
MemoryObjUse(pos, pat)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{pos}{specifying the environment as a position in the search list. Mostly there for back compatibility.}
  \item{pattern}{an optional regular expression. Only names matching pattern are returned. glob2rx can be used to convert wildcard patterns to regular expressions}

}
\details{
Objects use up memory in your R workspace.  
You can use this function to help with memory management.  For example, sort out redundant objects from a particular R programme, which are using up precious memory, and impove your overall memory management.  
}
\value{
A listing / table of objects being used and their associated memory in kilobytes(Kb) }
\references{
%% ~put references to the literature/web site here ~
}
\author{
Antonia Milkop <antonia.milkop@mbie.govt.nz>
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\code{\link{Memory}}
}
\examples{
vec1 <- 1:4
matrix1 <- as.matrix(vec1, nrow =2)
func1 <- function(x){x^2}
MemoryObjUse()
MemoryObjUse(pos = 1, pattern = "mat")
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{}
