\name{wrap}
\alias{wrap}
\title{
Insert line breaks
}
\description{
Inserts conveniently placed line breaks in a string or vector of strings.
}
\usage{
wrap(s, n, sep)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{s}{
Character string or vector of them to have line breaks inserted.
}
  \item{n}{
Maximum number of characters between line breaks. Default is 10.
}
  \item{sep}{
Line break format. Default is n. Also works for html's <br>.
}
}
\details{
A convenient function for formatting long strings for plot titles, table output, etc. without breaking up words.
}
\value{
A vector of the same length as s but with convenient linebreak characters inserted.
}
\references{
%% ~put references to the literature/web site here ~
}
\author{
Peter Ellis, generalised from xiechao at http://stackoverflow.com/questions/2351744/insert-line-breaks-in-long-string-word-wrap.
}
\note{
%%  ~~further notes~~
}



\seealso{

}
\examples{
#--------Toy example---------
wrap("A very long string here")
cat(wrap("A very long string here", 20))

#----------Example in a plot-----------
library(dplyr)
library(mbiedata)
data(IVStrips)

tmp <- IVStrips %>%
   group_by(COPRTop5) %>%
   summarise(
      Pop = sum(FinalWeight),
      Sample=length(FinalWeight)
      )

tmp$FacetLabels <- wrap(tmp$COPRTop5)

ggplot(tmp, aes(x=Sample, y=Pop)) +
  facet_wrap(~FacetLabels) +
  geom_point()

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }

