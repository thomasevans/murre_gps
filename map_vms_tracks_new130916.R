
# Updated 2013-10-14

# FUNCTIONS

length.real <- function(x) length(x[is.na(x) == FALSE])

rm(list = ls())

library(sp)
library(maptools)
library(animation)
library(zoo) # For interpolation
library(RColorBrewer)

# Shape file (world)
baltic <- readShapePoly(file.choose(), proj4string=CRS("+proj=longlat +ellps=clrk66"))  # folder New World under gis_data, file: level4.shp

# Vms tracks, 10-day period
vms <- readShapePoints(file.choose()) #vms.feb1-10.wholeFleet/vms.shp

# Remake time and date in R compatible format 
DateTime <- strptime(paste(as.character(vms@data[,"DATE"]), as.character(vms@data[,"TIME"])), "%d/%m/%Y %H:%M")
DateTime2 <- round(DateTime, "hour")

# Reshape data frame
plotdata1 <- data.frame(vms@data[,c("SIGNAL", "Speed")], DateTime2, vms@coords)
plotdata2 <- plotdata1[order(plotdata1[,"DateTime2"]),]
plotdata3 <- plotdata2[duplicated(paste(plotdata2[,"SIGNAL"], plotdata2[,"DateTime2"])) == FALSE, ]
plotdata4 <- reshape(plotdata3, timevar = "SIGNAL", idvar = "DateTime2", direction = "wide")

# Time vector for day and night - not very nice currently
a <- sin(seq(0.7,1,length.out = 13))
b <- rev(a[2:12])
time <- c(a, b)
times <- rep(time, 12)



# ANIMATION

# For Jonas picture
# if (i %in% 180:184) {
# image(x = test$image, col = paste(gray(seq(0.5,1, length.out = 50)), 50, sep = ""), xaxt = "n", yaxt = "n", bty = "n") } 
# else {


## WIDTH LINE
ani.options(ani.width = 750, ani.height = 750, interval = 0.08)
par(mar = c(0,0,0,0))
nvess <- (ncol(plotdata4)-1)/3
saveGIF({
for (i in 1:(nrow(plotdata4)-10))  {

# Map
plot(baltic, xlim = c(11, 20), ylim = c(53, 60))

# VMS tracks
for (j in 1:nvess) {

xdata <- plotdata4[i:(i+9),(j*3)]
# Stop loop if no data 
if (length.real(xdata) > 5) { 
xdata <- na.approx(xdata)
ydata <- plotdata4[i:(i+9),((j*3)+1)]; ydata <- na.approx(ydata) 
zdata <- plotdata4[i:(i+9),((j*3)-1)]; zdata <- na.approx(zdata) # speed
col <- ifelse(zdata > 5, "#000000", "#FF3333")
col[is.na(col) == TRUE] <- "#000000"
col2 <- paste(col, round(seq(10,99, length.out = 10),0), sep = "")
lwd2 <- ifelse(zdata > 5, 3.5, 12) 

for (k in 1:9) {  points(xdata[k:(k+1)], ydata[k:(k+1)], type = "l", col = col2[k], lwd = lwd2[k], pch = -1) }

}}

}
})


## PLOT non transparent line, problem: the shade out each other...
# reds <- brewer.pal(9, "Reds")
# blacks <- gray(round(seq(0.1,0.99, length.out = 9)))
# colsnew <- ifelse(zdata[2:length(zdata)] > 5, blacks, reds)


## OLD
ani.options(ani.width = 700, ani.height = 700, interval = 0.04)
par(mar = c(0,0,0,0))
nvess <- (ncol(plotdata4)-1)/3
saveGIF({
for (i in 1:10)  {
# (nrow(plotdata4)-10)

# Map
plot(baltic, xlim = c(11, 20), ylim = c(53, 60))

# VMS tracks
for (j in 1:nvess) {
xdata <- plotdata4[i:(i+9),(j*3)] 
ydata <- plotdata4[i:(i+9),((j*3)+1)] 
zdata <- plotdata4[i:(i+9),((j*3)-1)] # speed
col <- ifelse(zdata > 5, "#000000", "#FF3333")
col[is.na(col) == TRUE] <- "#000000"
col2 <- paste(col, round(seq(10,99, length.out = 10),0), sep = "")
cex2 <- ifelse(zdata > 5, 1.3, 2.5)
points(xdata, ydata, pch = 19, cex = cex2, col = col2, lwd = 0)
}
}
})


