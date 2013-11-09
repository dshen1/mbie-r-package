\name{qqNormEnv}
\alias{qqNormEnv}

\title{
QQNorm plot with envelope
}
\description{
Draw a QQ plot comparing a variable to a Normal distribution, while showing the envelope expected of a genuinely Normal distributed variable of the same sample size and moments.
}
\usage{
qqNormEnv(samp, ribbon.fill = alpha("steelblue", 0.1), ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{samp}{
A vector of data to be compared to a normal distribution
}
  \item{ribbon.fill}{
Colour of the fill to be used for the envelope drawn on the plot background
}
  \item{\dots}{
Other arguments to be passed through to qplot()
}
}
\details{
A good plot to compare an actual random variable to a hypothesised distribution is a qqplot.  The most widely used version is a QQ-norm plot (see qqnorm in base graphics), often used to check if residuals are plausibly normally distributed.  The analyst looks for deviations from a straight line to indicate there is an issue.  But the question arises - how much different from a straight line really shows that the data is unlikely to be normal?  The best way to assess this is by comparison with 19 genuinely normal random samples of the same size, mean and variance as the actual sample.  Simulation methods can be used to estimate the envelope that of values from such samples would occupy on a qqnorm plot and draw the envelope.
}
\value{
NULL
}
\references{
I nicked the basic algorithm from somewhere but can't find it now - I'd thought it was Venables & Ripley Modern Applied Statistics with S but can't find it in there.
}
\author{
Peter Ellis <peter.ellis@mbie.govt.nz>
}
\note{
Very similar functionality is provided by qqenvl from the envelope package, see http://www.inside-r.org/packages/cran/envelope/docs/qqenvl, although it's not clear where that package can be found.
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
qqNormEnv(rgamma(10, 1, 3))

qqNormEnv(rgamma(30, 1, 3))

qqNormEnv(runif(100))

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ htest }

