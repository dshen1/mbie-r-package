\name{RVM}
\alias{RVM}
\docType{data}
\title{
Regional Visitor Monitor
}
\description{
Regional Visitor Monitor 200X to 2011
}
\usage{data(RVM)}
\format{
 The data has been imported from SPSS format and has 38,750 observations on 3,578 variables.  Full names of these variables is stored elsewhere in a data dictionary (TODO - where?).  They are mostly factor or numeric.  The data is not efficiently structured; for example, there are multiple variables for specific activities in region X (eg Rotorua), even though five sixths of the respondents were interviewed in other regions.
 }
\details{
Data from interviews at tourist locations in Auckland, Canterbury, Dunedin, Rotorua, Queenstown and Wellington.  The population that inferences are being made to is ill-defined and hence there are no weights.
}
\source{
New Zealand Ministry of Business, Innovation and Employment
}
\references{
www.mbie.govt.nz
}

\seealso{

}

\examples{
data(RVM)

# Full variable names of variables
View(cbind(names(RVM), attributes(RVM)$variable.labels))

# RTO x how booked accommodation - cross tab
with(RVM, table(a20, a175))

# year x how booked accommodation - correspondence analysis
# (note - obvious how the survey changed in 2007)
require(ca)
plot(ca(with(RVM, table(a1, a175))))





}
\keyword{datasets}
