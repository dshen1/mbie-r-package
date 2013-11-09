\name{compare}
\alias{compare}
\title{
Compare the results of two outputs from \code{svyby()}
}
\description{
A rather dangerous little function that provides a back of the envelope comparison of two objects that have been produced by \code{svyby}.
}
\usage{
compare(a, b, color = tourism.cols(1), ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{a}{
First object, created by \code{svyby}.  The second column of \code{a} (and of \code{b}) should be a standard error of the point estimates in the first column.
}
  \item{b}{
Second object, created by \code{svyby}, for comparing to \code{a}.  Must be of exactly the same structure ie number of rows and columns.
}
  \item{color}{
Color of points and lines to be used in the dotplot created showing confidence intervals for the comparison of the two.
}
  \item{\dots}{
Additional arguments to be passed to \code{dotchartG()}.
}
}
\details{
Can be used to create and plot approximate confidence intervals of the difference between any two objects with the same number of rows where the first column is a point estimate and the second column a standard error.  Designed for use with objects created by \code{svyby()}.

This function is not highly recommended.  The statistical inference is crude; and the plot is generally inferior to one produced with \code{ggplot2}.  A preferred workflow, other than in "quick and dirty" situations, is to use more situation-specific inference of confidence intervals for differences, and plot the results using \code{ggplot}.
}
\value{
A dataframe of the same number of rows and rownames as \code{a} and \code{b}, with the following variables
\item{est }{Point estimate of the difference between the first column of \code{a} and first column of \code{b}}
\item{lower }{Lower bound of a confidence interval for \code{est}}
\item{upper }{Upper bound of a confidence interval for \code{est}}
As a side effect a dotchart is drawn with a grey background and white horizontal lines, with dots showing est and segments showing the confidence interval defined by upper and lower.
}
\references{
%% ~put references to the literature/web site here ~
}
\author{
Peter Ellis <peter.ellis@mbie.govt.nz>
}

\note{
Only works when a and b are created by a call to \code{svyby()} with only one variable, as otherwise \code{a} and \code{b} will not have the correct structure (first column to be point estimates, second column to be standard errors).

The confidence intervals created for this purpose are crude and should be treated as back of the envelope.  They rely on  \code{a} and \code{b} being independent; and asymptotic normality of the two sets of estimates.  They have not been corrected for multiple comparisons.
}

\seealso{
\code{\link{svyby}},  \code{\link{dotchartG}}, \code{\link{tourism.cols}}, \code{\link{confplot}}, \code{\link{ggplot}}
}
\examples{
library(survey)
# use example from IVS analysis

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ htest }
\keyword{ survey }

