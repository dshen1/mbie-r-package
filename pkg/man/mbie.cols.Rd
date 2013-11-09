\name{mbie.cols}
\alias{mbie.cols}

\title{
MBIE colours - function
}
\description{
Function to return a vector of colours approved in the MBIE style guide
}
\usage{
mbie.cols(x = 1:7)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
either a vector of any length of integers ranging from 1 to 7; or a character vector of any length of the words "Teal", "Green", "Blue", "Purple", "Pink", "Orange", "Yellow"; or a character string of "Duo1","Duo2, "Duo3", "Duo4", "Duo5",  "Trio1", "Trio2", "Trio3"}

}
\details{
If one of the Duo or Trio character strings is given to x, the value returned is 2 or 3 colours in one of the legitimate combinations specified in the style guide.  If x is a numeric vector or vector of colour names, the value returned as as would be expected.
}
\value{
A vector of colours in hexadecimal coding.}
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
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
mbie.cols("Teal")

mbie.cols("Trio3")

plotG(1:7, 1:7, col=mbie.cols(1:7), pch=19, cex=4)

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ utilities }

