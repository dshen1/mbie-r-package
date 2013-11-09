\name{Importance}
\alias{Importance}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Calculate "Importance" of each column in predicting a response variable.
}
\description{
Function returns the R-squareds for each column in x predicting the target.  If no target is given, uses the final column of x. Scale is recommended to be FALSE.  If true, it scales all the R-squared so they add to 100. 
}
\usage{
Importance(x, target = x[, ncol(x)], use = "pairwise.complete.obs", 
  Scale = FALSE, Plot = FALSE, Round = TRUE, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
A matrix or data frame of numeric variables, of which all (or all but one) are to be evaluated for their "importance" in predicting the target
}
  \item{target}{
A response variable, defaulting to the right-most variable of x
}
  \item{use}{
argument to be passed to cor() indicating what to do with missing data.  See helpfile for cor.
}
  \item{Scale}{
Logical value - should the resulting $R^2$ values be scaled so they add up to 100, giving the impression that between them the explanatory variables explain everything in the target?  
}
  \item{Plot}{
Should a pairwise plot be drawn, using ggpairs() from package GGally.
}
  \item{Round}{
Should R-squareds (which are multiplied by 100 ie from 0 to 100, not 0 to 1) be rounded to the nearest 1
}
  \item{\dots}{
Other arguments to be passed through to ggpairs if Plot=TRUE
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
A vector of numbers between zero and 100 with one element for each column of x being assessed for its importance in predicting target (ie correlation).  Values are the R-squared of a simple regression, multiplied by 100 to aid the simplistic interpretation of "percentage of target that is explained by each column of x".  If Scale=TRUE, values are forced to add to 100 regardless of how little of the variance in target they actually explain.}
\references{
%% ~put references to the literature/web site here ~
}
\author{
Peter Ellis <peter.ellis@mbie.govt.nz>
}
\note{

}

\section{Warning }{
Scaling the R-squareds so they all add up to 100 is what consultants used to do for the VEM to derive "importance" scores but it loses valuable information and makes the resulting scores somewhat misleading.  Capacity to do this has been built in for checking purposes.

In general, simply using correlation coefficients as a data-mining technique is not recommended.
}


\seealso{
\code{\link{cor}}
}
\examples{



}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
