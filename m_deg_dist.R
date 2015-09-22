# Based on deg_distj - but for list of points, calculating distance between each pair
# For first point it gives 0
# Returns in metres
m.deg.dist <- function(lat, long){
  
  n <- length(lat)
  lat1 <- lat[-n]
  lat2 <- lat[-1]
  long1 <- long[-n]
  long2 <- long[-1]
  
  source("deg.dist.R")
  
  # ?mapply
  dists <- mapply(deg.dist, long1 = long1, long2 = long2, lat1 = lat1, lat2 = lat2)
  dists <- c(0,dists)
  dists <- dists * 1000
  return(dists)
  
}