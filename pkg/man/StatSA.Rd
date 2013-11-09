\name{StatSA}
\alias{StatSA}
\docType{data}
\title{
proto object for stat_sa
}
\description{
Create \code{stat_sa()}.
}
\usage{stat_sa <- StatSA$new  }
\format{
  The format is:
proto object 
 $ calculate_groups:function (., data, scales, ...)   
 $ objname         : chr "seasadj" 
 $ default_geom    :function (.)   
 $ calculate       :function (., data, scales, frequency = 12, type = "multiplicative", index12.first = FALSE,  
    ...)   
 $ required_aes    : chr [1:2] "x" "y" 
 parent: proto object 
 .. parent: proto object 
}
\details{
This object operates in the background to create \code{stat_sa()} and does not need to be called by the user.
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
