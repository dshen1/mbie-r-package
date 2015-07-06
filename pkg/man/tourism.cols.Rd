\name{tourism.cols}
\alias{tourism.cols}

\title{
Tourism palette of colours - function
}
\description{
Return a vector of hexadecimal codes of colours from the "tourism" palette
}
\usage{
tourism.cols(x = c(1, 2, 3, 5, 12, 6))
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
Either a numeric vector of integers from 1 to 18; or the character string "Primary" which returns the same result as 1:6; or the character string "Secondary" which returns the same result as 7:18; or "All" which is the same as 1:18; or "Alternating" which is the same as rep(c(1, 6, 4, 8, 2, 16, 3, 5, 15, 18, 9, 14, 7, 12, 11, 17, 10, 13), 6) ie a set of colours that look reasonable alternating with eachother.
}
}
\details{

}
\value{
Hexadecimal codes of colours in line with the palette developed for the Ministry of Tourism around 2010.
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
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{

tourism.cols("Primary")

tourism.cols("Alternating")

plot(rep(1:6, 3), rep(1:3, each=6), type="n", bty="n", axes=F, 
  xlab="", ylab="", xlim=c(0,7), ylim=c(0,4),
  main="Tourism colours palette")
text(rep(1:6, 3), rep(1:3, each=6), label=1:18, col=tourism.cols(1:18), pch=19, cex=4)

library(dplyr)
library(scales)
library(ggplot2)
library(mbiedata)
data(IVStrips)

tmp <- IVStrips \%>\%
            group_by(YearQuarter, POVTop3, COPRTop5) \%>\%
            summarise(Total = sum(SmoothTotalSpend * FinalWeight * Adjusted_Factor))

tmp$period <- with(tmp, as.numeric(substring(YearQuarter, 1, 4)) + (as.numeric(substring(YearQuarter, 6, 6))-.5)/4)

ggplot(tmp, aes(x = period, y = Total, color = POVTop3)) +
  stat_sa(frequency = 4, geom = "point", size = 1) +
  stat_sa(frequency = 4, size = 1) +
  geom_line(frequency = 4, size = 0.5, alpha = 0.5) +
  scale_color_manual(values = tourism.cols("Alternating")) +
  facet_wrap(~COPRTop5, scale = "free_y") +
  ggtitle("Seasonally adjusted spend by purpose of visit")
  

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ utilities }

