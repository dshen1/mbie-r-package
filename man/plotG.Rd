\name{plotG}
\alias{plotG}

\title{
Grey background plot
}
\description{
A simple wrapper around plot() that for many basic plots will give a quick way to have a grey background, white gridlines and solid points - closer to the MBIE look and feel than the plot() defaults.
}
\usage{
plotG(..., col = tourism.cols(2), pch = 19, bty = "l", Fill = "grey95", xAdj = 1000, yAdj = 1000)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{\dots}{
Other arguments to pass through to plot, including at a minimum the object/s to be plotted but can also include other arguments such as type; see par() and plot() for details.
}
  \item{col}{
Colour of plotted glyphs; defaults to "Grass" or dark green
}
  \item{pch}{
Plotting character - see par() for details
}
  \item{bty}{
Box type - see par() for details
}
  \item{Fill}{
Colour of the background of the plotting area
}
  \item{xAdj}{
Adjustment to location of rectangle drawn on plotting area - allows manual fine tuning if necessary
}
  \item{yAdj}{
Adjustment to location of rectangle drawn on plotting area - allows manual fine tuning if necessary
}
}
\details{
The motivation for this function was to avoid having to draw a plot without a box, draw a grey rectangle over the plot area, add white gridlines, and re-draw the plot.  plotG() attempts to automate this process and generally speaking, often does so successfully.
}
\value{
NULL.  As a side product, produces a plot that should look similar to what is produced by plot() except for the solid points, "L" shaped box, grey background and white gridlines.
}
\references{
%% ~put references to the literature/web site here ~
}
\author{
Peter Ellis <peter.ellis@mbie.govt.nz>
}
\note{
Does not always work, but does actually work in a surprising number of circumstances.
}



\seealso{
\code{\link{dotchartG}}, \code{\link{plot}}
}
\examples{

# Basic usage
plotG(1:7, 1:7)

# Use with a time series, passing additional arguments through to plot
plotG(AirPassengers, type="l", lwd=2)



}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ hplot }

