\name{FormatDollars}
\alias{FormatDollars}
\title{
Format a number as dollars
}
\description{
Takes a numeric variable, round it and format it with dollar signs, commas as big markers, and option for a suffix like m.
}
\usage{
FormatDollars(x, endmark = "", ...) 
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
Numeric variable to be formatted
}
  \item{endmark}{
Character to be added to the end of each element eg "m" for millions
}
  \item{...}{
Other arguments to be passed through to \code{round()}
}
}
\details{

}
\value{
A character vector of the same length as x suitable for use in a table or plot annotation.
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



\seealso{

}
\examples{
#--------Toy examples---------
FormatDollars(1.5:10.5, "m")
FormatDollars(1.5:10.5, "m", digits=1)


}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~character }

