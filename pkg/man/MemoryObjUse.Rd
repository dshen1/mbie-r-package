\name{MemoryObjUse}
\alias{MemoryUse}
\alias{MemoryObjectUse}
\alias{ObjectMemoryUse}
\alias{ObjMemoryUse}

\title{
Size of memory used by objects - function
}
\description{
Function to return a list of objects currently being used by R, and their associated memory use (Kb)
}
\usage{
MemoryObjUse()
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{}

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
MemoryObjUse()

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ memory, memory usage, memory management }
