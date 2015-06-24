data(forecasts)

final <- subset(forecasts, Year == 2019)[, c("Country", "TotalVisitorSpend")]

forecasts$Country <- factor(forecasts$Country, levels = final$Country[order(-final$TotalVisitorSpend)])

ggplot(subset(forecasts, Year >= 1998), aes(x = Year, y = TotalVisitorSpend, colour = Country, linetype = Type)) +
   geom_line(size = 1) +
   scale_y_continuous(label = dollar) +
   labs(x = "", y = "Total visitor spend ($m)\n", title = "Tourism forecasts, New Zealand") +
   scale_color_manual(values = tourism.cols("Alternating"))


ggplot(subset(forecasts, Year >= 1998), aes(x = TotalVisitorSpend, y = TotalVisitorDays / 10^6, colour = Year)) +
   geom_text(aes(label = Year), size = 3) +
   geom_path() +
   facet_wrap(~Country, scale = "free")