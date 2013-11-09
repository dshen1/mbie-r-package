\name{rename.levels}
\alias{rename.levels}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Rename factor levels
}
\description{
A little convenience function to rename levels in a factor; can also be used to merge levels together
}
\usage{
rename.levels(x, orig, new)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
A vector of class factor
}
  \item{orig}{
a character vector of the original levels of the factor (not necessarily all its levels) that need to be changed
}
  \item{new}{
a character vector with the same number of elements as orig or the new values to change the levels to
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
A factor with the same structure as x but with different levels
}
\references{
%% ~put references to the literature/web site here ~
}
\author{
Peter Ellis <peter.ellis@mbie.govt.nz>
}
\note{
Similar functionality can be found in other packages but this function was convenient for a number of repetitive tasks we had to perform in 2012 and has found its way into our overall dialect of R.
}


\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
data(IVStrips)
IVStrips$POV2 <- rename.levels(IVStrips$POVArrival,
                               orig=c("Conference/Convention", "Education", "Dont Know"),
                               new=c("Business",                "Ed.",    "Other"))

with(IVStrips, table(POVArrival, POV2))
}

\keyword{ category }
\keyword{ manip }
