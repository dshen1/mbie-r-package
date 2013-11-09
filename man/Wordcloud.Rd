\name{Wordcloud}
\alias{Wordcloud}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Wordcloud and text mining wrapper
}
\description{
Import one or more text files, perform basic text reduction, and draw a wordcloud.
}
\usage{
Wordcloud(directory, colors = tourism.cols(12:7), ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{directory}{
Location of folder containing text files to import.
}
  \item{colors}{
Colours to use for words.
}
  \item{\dots}{
Other arguments to be passed through to \code{wordcloud()}
}
}
\details{
This is a simple wrapper around some functions in the \code{tm} and \code{wordcloud} packages, to allow users to draw a wordcloud from a text file without any familiarity with text mining functions.
}
\value{
NULL
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

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\code{\link[tm]{Corpus}}, \code{\link[tm]{tm_map}}, \code{\link[tm]{TermDocumentMatrix}}, \code{\link[wordcloud]{wordcloud}},
}
\examples{
\dontrun{
    require(wordcloud)
    require(tm)
    
    # Draw a wordcloud based on all text files in the directory below:
    Wordcloud("P:OTSP/IVS/Wordclouds")
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
