
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