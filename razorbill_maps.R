# Mapping razorbill tracks


# set working directory (where all the files should be located),
# both those to load in and where to output the figures.
# Change this to wherever you have your files
# remember to use the back-slashes (\) not forward (/)
setwd("D:\Dropbox\R_projects\razos")

# Load in razorbill GPS data -----
load(file = "razo_gps_all.RData")




# Alpha channel function ----
# Transparency to display overlapping points
addalpha <- function(colors, alpha=1.0) {
  r <- col2rgb(colors, alpha=T)
  # Apply alpha
  r[4,] <- alpha*255
  r <- r/255.0
  return(rgb(r[1,], r[2,], r[3,], r[4,]))
}


# Function to plot a base map ----
library(maps)
library(RColorBrewer)

# Coastline data
load("SWE_adm0.RData")

# Reduce ouput maps file size by clipping coastline data to
# only map area required 
library(raster)
gadm_clip <- crop(gadm, extent(range(gps_all$long)[1]-0.5,
                               range(gps_all$long)[2]+0.5,
                               range(gps_all$lat)[1]-0.5,
                               range(gps_all$lat)[2]+0.5))

map.base.fun <- function(xlim = c(17,18.3), ylim =  c(57,57.7)){
  par(mfrow=c(1,1))
  par( mar = c(5, 4, 4, 2))

  # How map appears, col gives the fill colour
  # Lty and lwd give the line type (e.g. broken or solid) and line width
  plot(gadm_clip, xlim = xlim,
       ylim = ylim, col= "dark grey", bg = NA,
       lty = 1,
       lwd = 1)
  
  ## Scale bar and axis
  box(lwd=3)
  axis(side=(1),las=1)
  axis(side=(2),las=1)
  
}


# hack map.scale function
# map.scale2 <- map.scale
# fix(map.scale2)
source("map.scale2.R")



# Make a vector of birds
murre_ids <- unique(gps_all$bird_id)

# # Use if want colours
col.vec <- rainbow(length(murre_ids))
col.vec.al <- addalpha(col.vec, alpha = 0.3)
# # To randomize colour order
# # For repeatable 'random' order
set.seed(1)
col.vec.al.rand <- col.vec.al[sample(seq_along(col.vec.al))]
# 
# # # If don't want colours
# # col.vec.al.rand <- addalpha("black", alpha = 0.4)
# # col.vec.al.rand <- rep(col.vec.al.rand, length(murre_ids))


# Remove 2010 data

# Label trip_ids ---------
# First calculate distance of points from colony
# Then set some threshold distance overwhich GPS locations
# are classified as being on a trip.
# Individual trips are then labelled from these.


# Calculate distance from colony
source("deg.dist.r")

gps_all$col.dist <- mapply(deg.dist,
                           long1 = 17.9642739, lat1 = 57.2855572,
                           long2 = gps_all$long, lat2 = gps_all$lat)

hist(gps_all$col.dist, breaks = 500, xlim = c(0,5))
abline(v = c(0.5, 1))
# Remove locations <1 km from island centre???

# Set threshold distance as 1.5 km
dist_over <- gps_all$col.dist >1.5

# This labels points as:
# 0 - Not on trip
# 1 - last point of trip
# 3 - Middle points of trip
# 2 - first point of trip
dist_over_1 <- dist_over[-1] + dist_over[-1] + dist_over[-length(dist_over)]
dist_over_1 <- c(dist_over_1, 0)
summary(factor(dist_over_1))

# Give unique numbers to each foraging trip
trip_id <- NULL
trip_id_count <-0
for(i in 1:nrow(gps_all)){
  if(dist_over_1[i] == 0){trip_id[i] <- 0}else{
    if(dist_over_1[i] == 2) {
      trip_id_count <- trip_id_count +1
    trip_id[i] <- trip_id_count}
  else{trip_id[i] <- trip_id_count}
  
  }
}

gps_all$trip_id <- trip_id
summary(factor(gps_all$trip_id))





# Plot map of trips ----

# Only produce one file type at a time (i.e. only one of the lines
# png, svg, or pdf). Svg and pdf are vector formats, so are better
# for resizing.
resa = 72*4
png("razo_gps_tracks_col_trips.png", res = resa, width = 5*resa, height = 5*resa)
svg("razo_gps_tracks_col_trips.svg",width = 5, height = 5)
pdf("razo_gps_tracks_col_trips.pdf",width = 5, height = 5)

# Replace zeros with NA
gps_all$trip_id[gps_all$trip_id == 0] <- NA
summary(factor(gps_all$trip_id))


map.base.fun(xlim = range(gps_all$long), ylim = range(gps_all$lat))

# cols <- rainbow(n = max(gps_all$trip_id, na.rm = TRUE))
# cols <- addalpha(cols, 0.7)
# 
# # Shuffle
# set.seed(1)
# cols <- sample(cols, max(gps_all$trip_id, na.rm = TRUE))

# Trip line types
set.seed(1)
lty.t <- sample(1:6, length(unique(gps_all$trip_id)),
                replace = TRUE)

# Map each bird in turn
# i <- 4
for(i in 1:length(murre_ids)){
  
  x <- murre_ids[i]
  # ?subset
  gps.sub <- gps_all[gps_all$bird_id == x  &
                       !is.na(gps_all$trip_id),]
  n <- length(gps.sub$long)
  segments(gps.sub$long[-1], gps.sub$lat[-1],
           gps.sub$long[1:n-1], gps.sub$lat[1:n-1],
           col = col.vec.al.rand[i],
           lty = lty.t[gps.sub$trip_id[-1]],
           lwd = 1.5)
}

# col.vec.al.rand[gps.sub$trip_id]
# unique(gps_all$trip_id)

# Add map scale bar
map.scale2(ratio = FALSE, lwd.line = 2,
           relwidth = 0.25, cex = 1.2)

dev.off()
