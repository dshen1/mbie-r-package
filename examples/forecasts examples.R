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