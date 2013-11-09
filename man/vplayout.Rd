\name{vplayout}
\alias{vplayout}
\title{
Place plot in grid layout.
}
\description{
Utility to help place grid graphics including from ggplot on locations on a device
}
\usage{
vplayout(x, y)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
Horizontal coordinate to place a grob on the grid previously set up by \code{pushViewport(viewport())}
}
  \item{y}{
Vertical coordinate to place a grob on the grid previously set up by \code{pushViewport(viewport())}
}
}
\details{
Provides similar functionality for grid graphics including those created by \code{ggplot}, to  what \code{layout{}} provides in base graphics.  For use after a call to \code{pushViewport(viewport(layout = grid.layout(...)))} - see example.
}
\value{
A viewport for use in grid graphics commands, for example in \code{print.ggplot}.
}
\references{
%% ~put references to the literature/web site here ~
}
\author{
Hadley Wickham (I think) - taken from his book \emph{ggplot2 - elegant graphics}, or whatever its correct title is
}
\note{
%%  ~~further notes~~
}



\seealso{
\code{\link{pushViewport}}, \code{\link{viewport}}, \code{\link{print.ggplot}}
}
\examples{
# Create example plots and store as a, b, c
a <- qplot(1:10, rnorm(10), main = "a")
b <- qplot(1:10, rnorm(10), main = "b")
c <- qplot(1:10, rnorm(10), main = "c")

# Set up page to print plots to, with a 2 x 2 layout
grid.newpage()
pushViewport(viewport(layout = grid.layout(2, 2)))

# print to various locations on the layout
print(a, vp = vplayout(1, 1:2))  # First row and both columns
print(b, vp = vplayout(2, 1))    # Second row and first column ie bottom left
print(c, vp = vplayout(2, 2))    # Bottom right



}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
