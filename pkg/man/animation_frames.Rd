\name{animation_frames}
\alias{animation_frames}
\docType{data}
\title{
animation_frames
}
\description{
Function that takes an existing ggplot object and redraws it as many images, for use in an animation
}
\usage{animation_frames(plot, dev = "CairoPDF", start = 1, folder = "animations", filename = "image", method = "by x", ...)}
 
\arguments{
  \item{plot}{a plot created by ggplot}
\item{dev}{either CairoPDF or png}
\item{start}{the number of points to start with in the first frame}
\item{folder}{folder in which to save all the images.  Folder "animations" will be created if one does not exist already}
\item{filename}{the kernal of the filename for each image}
\item{method}{If each frame represents adding a row of data, use "by row".  Or if each frame is an increment of one value on the x axis, use "by x".  
"By x" will work best with time series more complex than a single line (e.g. if you have colour or facet mapped to a variable).  
"By row" will work best for a scatter plot.}
\item{...}{other arguments to be passed to CairoPDF or png, e.g. width = 9, height = 7}

 }
\details{
Each image adds one more point, moving down the data in their original order.  So the order of data frame that was used to create the original plot is important.  For meaningful results, the original plot needs to explicitly set the y and x limits, so each frame has the same scale.
}
\source{
New Zealand Ministry of Business, Innovation and Employment
}
\references{
www.mbie.govt.nz
}

\seealso{
NULL
}

\examples{
# We have included the dontrun{} command in all the animation_frames() examples to save people having to click 'enter' multiple times if they automatically run the example(animation_frames). \
# If you want to run the examples, please copy them from here and run separately in your own R script

\dontrun{

# Example 1 - scatterplot with a linear regression line through it
plot1 <- ggplot(mtcars, aes(x=disp, y=mpg)) + geom_point() + geom_smooth(method="lm") + ylim(0, 100) + xlim(0, 500) 
animation_frames(plot1, start = 2, dev="png", method = "by row")


# Example 2 - simple time series line plot
data(AirPassengers)
AirPassengers_df <- data.frame(AirPassengers)
AirPassengers_df$Period <- time(AirPassengers)
plot2 <- ggplot(AirPassengers_df, aes(x=Period, y=AirPassengers)) + geom_line() + xlim(1948, 1962) + ylim(0, 700)
animation_frames(plot2, start = 10, dev="png", filename="AirPassengers")

# Example 3 - more complex time series
library(Cairo)
library(dplyr)
data(IVStrips)
tmp2 <- IVStrips %>%
         group_by(YEJun, POVTop3, COPRTop5) %>%
         summarise(Spend = sum(FinalWeight * SmoothTotalSpend * Adjusted_Factor)) %>%
         data.frame()

plot3 <- ggplot(tmp, aes(x=YEJun, y = Spend / 10 ^ 6, colour=COPRTop5)) +
  facet_wrap( ~ POVTop3) +
	geom_line() +
	scale_colour_manual(values=tourism.cols()) +
	scale_y_continuous("Total tourist spend ($m)\n", label=comma, limits=c(0, 1.7 * 10 ^ 3)) +
	scale_x_continuous("", limits=c(1997, 2014)) +
	theme_bw(base_family="Calibri")
animation_frames(plot3, filename="IVS", width=9, height=7)
}
}
\keyword{animation_frames}


\author{
Peter Ellis <peter.ellis@mbie.govt.nz> 
10 September 2014
}
