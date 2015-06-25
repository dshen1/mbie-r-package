
# Example 1 - scatterplot with a linear regression line through it
plot1 <- ggplot(mtcars, aes(x = disp, y = mpg)) +
   geom_point() +
   geom_smooth(method = "lm") +
   ylim(0, 100) +
   xlim(0, 500) 

   animation_frames(plot1, start = 2, dev = "png", method = "by row")


# Example 2 - simple time series line plot
data(AirPassengers)
AirPassengers_df <- data.frame(AirPassengers)
AirPassengers_df$Period <- time(AirPassengers)

plot2 <- ggplot(AirPassengers_df, aes(x = Period, y = AirPassengers)) +
   geom_line() +
   xlim(1948, 1962) +
   ylim(0, 700)

   animation_frames(plot2, start = 10, dev = "png", filename = "AirPassengers")


# Example 3 - more complex time series
library(Cairo)
library(dplyr)
library(mbiedata)
data(IVStrips)

tmp <- IVStrips %>%
         group_by(YEJun, POVTop3, COPRTop5) %>%
         summarise(Spend = sum(FinalWeight * SmoothTotalSpend * Adjusted_Factor))

plot3 <- ggplot(tmp, aes(x = YEJun, y = Spend / 10 ^ 6, colour = COPRTop5)) +
   facet_wrap( ~ POVTop3) +
	geom_line() +
	scale_colour_manual(values = tourism.cols()) +
	scale_y_continuous("Total tourist spend ($m)\n", label = comma, limits = c(0, 1.7 * 10 ^ 3)) +
	scale_x_continuous("", limits = c(1997, 2014)) +
	theme_bw(base_family = "Calibri")
   
   animation_frames(plot3, filename = "IVS", width = 9, height = 7)
