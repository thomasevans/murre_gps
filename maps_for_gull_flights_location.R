
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

gadm_clip <- crop(gadm, extent(5,30,50,65))

map.base.fun <- function(xlim = c(10,25), ylim =  c(55,62)){
  par(mfrow=c(1,1))
  par( mar = c(5, 4, 4, 2))
  
  # How map appears, col gives the fill colour
  # Lty and lwd give the line type (e.g. broken or solid) and line width
  plot(gadm_clip, xlim = xlim,
       ylim = ylim, col= "dark grey", bg = NA,
       lty = 1,
       lwd = 1)
  
  ## Scale bar and axis
  box(lwd=2)
  axis(side=(1),las=1, tcl=0.5, padj = -1)
  axis(side=(2),las=1, tcl=0.5, hadj = 0.1)
  axis(side=(3),las=1, tcl=0.5, padj = 1.2)
  axis(side=(4),las=1, tcl=0.5, hadj = 0.8)
  
}


# hack map.scale function
# map.scale2 <- map.scale
# fix(map.scale2)
source("map.scale2.R")


# Plot map of trips ----

# Only produce one file type at a time (i.e. only one of the lines
# png, svg, or pdf). Svg and pdf are vector formats, so are better
# for resizing.
# resa = 72*4
# png("overview_map.png", res = resa, width = 5*resa, height = 5*resa)
# svg("overview_map.svg",width = 5, height = 5)
# pdf("overview_map.pdf",width = 5, height = 5)
postscript("overview_map.ps",width = 5, height = 5)


map.base.fun()

cols <- rainbow(n = max(gps_all$trip_id, na.rm = TRUE))
cols <- addalpha(cols, 0.7)

# Shuffle
set.seed(1)
cols <- sample(cols, max(gps_all$trip_id, na.rm = TRUE))

# Map each bird in turn
# i <- 4
for(i in 1:length(murre_ids)){
  
  x <- murre_ids[i]
  # ?subset
  gps.sub <- gps_all[gps_all$bird_id == x,]
  n <- length(gps.sub$long)
  segments(gps.sub$long[-1], gps.sub$lat[-1],
           gps.sub$long[1:n-1], gps.sub$lat[1:n-1],
           col = cols[gps.sub$trip_id], lty = 1, lwd = 1)
}

# Add map scale bar
map.scale2(ratio = FALSE, lwd.line = 2,
           relwidth = 0.25, cex = 1.2)

dev.off()
