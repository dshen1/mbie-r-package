\name{plot_map}
\alias{plot_map}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Centered world map
}
\description{
Draw a map of the world centered at an arbitrary longitude without the annoying horizontal lines that usually occur when polygons cross 180 degrees longitude
}
\usage{
plot_map(database, center, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{database}{
A map database from the maps package, usually "world"
}
  \item{center}{
Longitude on which to center the map
}
  \item{\dots}{
Other arguments to pass through to map()
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
NULL
}
\references{
http://stackoverflow.com/questions/5353184/fixing-maps-library-data-for-pacific-centred-0-360-longitude-display
}
\author{
Joris Meys
}
\note{
Shamelessly nicked from a stackoverflow post.
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{

# Draw a world map centered on New Zealand
plot_map("world", center=180, col="white", bg="gray",
   fill=TRUE, ylim=c(-60,90), mar=c(0,0,0,0))
   }
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ hplot }

