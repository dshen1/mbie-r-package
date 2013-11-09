\name{StatRollApplyR}
\alias{StatRollApplyR}
\docType{data}
\title{
proto object for stat_rollapplyr
}
\description{
Create \code{stat_rollapplyr()}.
}
\usage{stat_rollapplyr <- StatRollApplyR$new }
\format{
  The format is:
proto object 
 $ calculate_groups:function (., data, scales, ...)   
 $ objname         : chr "rollapplyr" 
 $ default_geom    :function (.)   
 $ calculate       :function (., data, scales, width = 12, FUN, fill = NA, align = "right", index12.first = FALSE,  
    ...)   
 $ required_aes    : chr [1:2] "x" "y" 
 parent: proto object 
 .. parent: proto object 
}
\details{
This object operates in the background to create \code{stat_rollapplyr()} and does not need to be called by the user.
}
\source{
%%  ~~ reference to a publication or URL from which the data were obtained ~~
}
\references{
%%  ~~ possibly secondary sources and usages ~~
}
\examples{

}
\keyword{datasets}
