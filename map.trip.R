
# Base map to plot different types of points on -----
# function to create a simple map, high-lighting flight and non-flight only
map.trip <- function(points = points, xlim = NULL, ylim = NULL){
  
  library(maps)
  
  p <- points[points$long != 0,]
  
  # plot base map
  # Set map limits
  c.xlim <- range(p$long, na.rm = TRUE)
  dif    <- c.xlim[2] - c.xlim[1]
  dif    <- dif *.12
  c.xlim <- c((c.xlim[1] - dif), (c.xlim[2] + dif))
  
  #   xlim = NULL
  
  if(!is.null(xlim)){
    c.xlim <- xlim
  }
  
  c.ylim <- range(p$lat, na.rm = TRUE)
  dif    <- c.ylim[2] - c.ylim[1]
  dif    <- dif *.1
  c.ylim <- c((c.ylim[1] - dif), (c.ylim[2] + dif))
  
  if(!is.null(ylim)){
    c.ylim <- ylim
  }
  
  # Plot base map
  load("SWE_adm0.RData")
  
  par(mfrow=c(1,1))
  par( mar = c(5, 4, 4, 2))
  #   par(bg = 'white')
  
  plot(gadm, xlim = c.xlim,
       ylim = c.ylim, col="grey70", bg = "gray90",
       #      xlab = "Latitude"
  )
  
  n <- length(p$lat)
  
  # Plot lines between points
  segments(p$long[-1], p$lat[-1],
           p$long[1:n-1], p$lat[1:n-1],
           col = "black", lty = 1, lwd = 1.5)
  
  # Plot points  
  points(p$lat~p$long,
         col = "blue", cex = 0.7)
  
  points(p$lat[p$speed > 5]~
           p$long[p$speed > 5],
         col = "red", cex = 0.7)
  
  
  # Scale bar and axis
  x <- c.xlim[1] + ((c.xlim[2] - c.xlim[1])/20)
  y <- c.ylim[1] + ((c.ylim[2] - c.ylim[1])/10)
  map.scale(x, y, ratio = FALSE, col = "grey60",
            relwidth = 0.25,
            col.lab = "grey60")
  
  box(,col="grey50",lwd=2)
  axis(side=(1), las=1, col="grey60", col.axis="grey50")
  axis(side=(2), las=1, col="grey60", col.axis="grey50")
  
}
