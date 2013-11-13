\name{forecasts}
\alias{forecasts}
\alias{forecasts}
\docType{data}
\title{
New Zealand tourism forecasts 2013 - 2019
}
\description{
New Zealand tourism forecasts 2013 - 2019
}
\usage{data(forecasts)}
\format{
 Semi-normalised data.  Provides total spend, spend per day, total visitors, total visitor days, and visitor arrivals for business, visiting friends and relatives, holiday and other purpose of visit.  Data from 2000 to 2012 are actuals; from 2013 onwards they are forecasts
 
 }
\details{
These forecasts are produced for MBIE by NZIER and are based on factor-augmented vector autoregression models.
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

data(forecasts)

final <- subset(forecasts, Year==2019)[, c("Country", "Total.Visitor.Spend")]

forecasts$Country <- factor(forecasts$Country, levels=final$Country[order(-final$Total.Visitor.Spend)])

ggplot(forecasts, aes(x=Year, color=Country, linetype=Type, y=Total.Visitor.Spend / 10^6)) +
  geom_line(size=2) +
  labs(x="", y="Total visitor spend ($m)\n", title="Tourism forecasts, New Zealand") +
  scale_color_manual(values=tourism.cols("Alternating"))


ggplot(forecasts, aes(x=Total.Visitor.Spend / 10^6, y=Total.Visitor.Days / 10^6, color=Year)) +
  geom_text(aes(label=Year), size=3) +
  geom_path() +
  facet_wrap(~Country, scale="free")

}
\keyword{datasets}
