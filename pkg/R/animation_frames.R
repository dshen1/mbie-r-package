

animation_frames <- function(plot, 
                             dev      = "CairoPDF", 
                             start    = 1, 
                             folder   = "animations", 
                             filename = "image",
							 method = "by x",
                             ...){
  ## For debugging
  # plot <- ggplot(mtcars, aes(x=disp, y=mpg)) + geom_point() + geom_smooth() + ylim(0, 100) + xlim(0, 500)
  # plot <- ggplot(AirPassengers_df, aes(x=Period, y=AirPassengers)) + geom_line() + xlim(1948, 1962) + ylim(0, 700) # alternative
  # start <- 2; dev <- "png"; folder <- "animations"; filename <- "image"
  
  # Author: Peter Ellis, 10/9/2014
  # Function that takes an existing ggplot object and redraws it as many images
  # for use in an animation.  Each image adds one more point, moving down
  # the data in their original order.  So the order of the data frame that
  # was used to create the original plot is important.
  #
  # For meaningful results, the original plot needs to explicitly set the y and 
  # x limits, so each frame has the same scale.
  #
  # Variables:
  # plot: a plot created by ggplot()
  # dev: either CairoPDF or png
  # start: the number of points to start with in the first frame
  # folder: folder in which to save all the images.  folder will be created if it does not exist.
  # filename: the kernel of the filename for each image.
  # method:  does each frame represent adding a row of data ("by row") or an increment of one value on the x axis ("by x").  "By x" will work best
  #          with time series more complex than a single line (eg if you have colour or facet mapped to a variable).  "By row" will work best for a
  #          scatter plot.
  # ...: other arguments to be passed to CairoPDF or png, eg width and height
  
  
  
  dir.create(folder, showWarnings = FALSE)
  
  if (method == "by row"){
	  for(i in start : nrow(plot$data)){
		if(dev == "CairoPDF"){
		  CairoPDF(paste0(folder, "/", filename, i, ".pdf"), ...)
		} else
		  if(dev == "png"){
			png(paste0(folder, "/", filename, i, ".png", ...))
		  } else  {
			stop("Only CairoPDF and png devices supported")
		  }
		this_data <- plot$data[1:i, ]
		this_plot <- plot %+% this_data
		print(this_plot)
		dev.off()
		}
	  	n <- c("Number of images" = nrow(plot$data) - start + 1)
		
	  } else if (method == "by x"){
	  
		xvar <- as.character(plot$mapping$x)
		xs <- unique(plot$data[ , xvar])
		for(i in start : length(xs)){
			if(dev == "CairoPDF"){
			  CairoPDF(paste0(folder, "/", filename, i, ".pdf"), ...)
			} else
			  if(dev == "png"){
				png(paste0(folder, "/", filename, i, ".png", ...))
			  } else  {
				stop("Only CairoPDF and png devices supported")
			  }
			this_data <- plot$data[plot$data[ ,xvar] %in% xs[1:i], ]
			this_plot <- plot %+% this_data
			print(this_plot)
			dev.off()
		}
		n <- c("Number of images" = length(xs) - start + 1)  
	  
	  } else {
		stop("method must be 'by x' or 'by row'")
	  }
	return(n)  
}