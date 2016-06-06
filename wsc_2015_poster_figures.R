
# Figures for WSC murre foraging poster - for GPS data

#' 1. Maps for 3 years of foraging tracks
#' 2. Histogram of distance from colony (foraging locations only, weighted by time-interval between GPS fixes)
#' 3. Histogram of water depth at foraging 'locations'
#'    - Need to then extract water depth for all GPS location - do in new script



# Get GPS data for murres for the 3 years - linked with foraging trip ID ------
#' Paramaters required:
#' - Device_info_serial
#' - Date_time
#' - Lat + long
#' - Distance from colony
#' - Point classification (behaviour type)
#' - Point classification (whether on trip)
#' - Trip_id
#' - Ring_number

library("RODBC")

# Connect to DB
gps.db <- odbcConnectAccess2007('D:/Dropbox/tracking_db/murre_db/murre_db.accdb')




# Get uva data
gpspoints.uva <- sqlQuery(gps.db,
                          query = "SELECT uvabits_gps.device_info_serial, uvabits_gps.date_time, uvabits_gps.latitude, uvabits_gps.longitude, guillemots_gps_points_trip_id.trip_id, guillemots_gps_points_trip_id.deploy_id, guillemots_gps_points_flight_id.flight_id, guillemots_gps_points_movebank_ECMWF_calc.wind_dir, guillemots_gps_points_components_wind.ground_speed, guillemots_gps_points_movebank_ECMWF_calc.wind_speed_10m, guillemots_gps_points_components_wind.alpha, guillemots_gps_points_components_wind.wind_side_10, guillemots_gps_points_components_wind.wind_head_tail_10, guillemots_gps_points_components_wind.wind_side, guillemots_gps_points_components_wind.wind_head_tail, guillemots_gps_points_movebank_ecmwf.ecmwf_cloud_cov_tot, guillemots_gps_points_movebank_ecmwf.ecmwf_pressure_sea_lev, guillemots_gps_points_movebank_ecmwf.ecmwf_temp_2m, guillemots_gps_points_components_wind.head_dir_ecmwf, guillemots_gps_points_components_wind.head_speed_ecmwf, guillemots_gps_points_movebank_ECMWF_calc.wind_speed_flt_ht, guillemots_gps_points_uva_class.coldist, guillemots_track_session.ring_number
                          FROM guillemots_track_session, (((guillemots_gps_points_flight_id INNER JOIN (guillemots_gps_points_trip_id INNER JOIN (guillemots_gps_points_uva_class INNER JOIN uvabits_gps ON (guillemots_gps_points_uva_class.device_info_serial = uvabits_gps.device_info_serial) AND (guillemots_gps_points_uva_class.date_time = uvabits_gps.date_time)) ON (guillemots_gps_points_trip_id.date_time = uvabits_gps.date_time) AND (guillemots_gps_points_trip_id.device_info_serial = uvabits_gps.device_info_serial)) ON (guillemots_gps_points_flight_id.date_time = guillemots_gps_points_trip_id.date_time) AND (guillemots_gps_points_flight_id.device_info_serial = guillemots_gps_points_trip_id.device_info_serial)) INNER JOIN guillemots_gps_points_movebank_ECMWF_calc ON (uvabits_gps.device_info_serial = guillemots_gps_points_movebank_ECMWF_calc.device_info_serial) AND (uvabits_gps.date_time = guillemots_gps_points_movebank_ECMWF_calc.date_time)) INNER JOIN guillemots_gps_points_components_wind ON (guillemots_gps_points_movebank_ECMWF_calc.device_info_serial = guillemots_gps_points_components_wind.device_info_serial) AND (guillemots_gps_points_movebank_ECMWF_calc.date_time = guillemots_gps_points_components_wind.date_time)) INNER JOIN guillemots_gps_points_movebank_ecmwf ON (guillemots_gps_points_components_wind.device_info_serial = guillemots_gps_points_movebank_ecmwf.device_info_serial) AND (guillemots_gps_points_components_wind.date_time = guillemots_gps_points_movebank_ecmwf.date_time)
                          WHERE (((uvabits_gps.date_time)>=[guillemots_track_session].[start_date] And (uvabits_gps.date_time)<=[guillemots_track_session].[end_date]) AND ((uvabits_gps.latitude)<>0) AND ((uvabits_gps.longitude)<>0) AND ((guillemots_track_session.device_info_serial)=[uvabits_gps].[device_info_serial]))
                          ORDER BY uvabits_gps.device_info_serial, uvabits_gps.date_time;
                          ",
                          as.is = TRUE)


# Get igu data
gpspoints.igu <- sqlQuery(gps.db,
                          query = "SELECT guillemots_gps_points_igu.device_info_serial, guillemots_gps_points_igu.date_time, guillemots_gps_points_igu.latitude, guillemots_gps_points_igu.longitude, guillemots_gps_points_trip_id.trip_id, guillemots_gps_points_trip_id.deploy_id, guillemots_gps_points_flight_id.flight_id, guillemots_gps_points_movebank_ECMWF_calc.wind_dir, guillemots_gps_points_components_wind.ground_speed, guillemots_gps_points_movebank_ECMWF_calc.wind_speed_10m, guillemots_gps_points_components_wind.alpha, guillemots_gps_points_components_wind.wind_side_10, guillemots_gps_points_components_wind.wind_head_tail_10, guillemots_gps_points_components_wind.wind_side, guillemots_gps_points_components_wind.wind_head_tail, guillemots_gps_points_movebank_ecmwf.ecmwf_cloud_cov_tot, guillemots_gps_points_movebank_ecmwf.ecmwf_pressure_sea_lev, guillemots_gps_points_movebank_ecmwf.ecmwf_temp_2m, guillemots_gps_points_components_wind.head_dir_ecmwf, guillemots_gps_points_components_wind.head_speed_ecmwf, guillemots_gps_points_movebank_ECMWF_calc.wind_speed_flt_ht, guillemots_gps_points_igu_class.coldist, guillemots_track_session.ring_number
                          FROM guillemots_track_session, (((guillemots_gps_points_flight_id INNER JOIN (guillemots_gps_points_trip_id INNER JOIN (guillemots_gps_points_igu_class INNER JOIN guillemots_gps_points_igu ON (guillemots_gps_points_igu_class.device_info_serial = guillemots_gps_points_igu.device_info_serial) AND (guillemots_gps_points_igu_class.date_time = guillemots_gps_points_igu.date_time)) ON (guillemots_gps_points_trip_id.date_time = guillemots_gps_points_igu.date_time) AND (guillemots_gps_points_trip_id.device_info_serial = guillemots_gps_points_igu.device_info_serial)) ON (guillemots_gps_points_flight_id.date_time = guillemots_gps_points_trip_id.date_time) AND (guillemots_gps_points_flight_id.device_info_serial = guillemots_gps_points_trip_id.device_info_serial)) INNER JOIN guillemots_gps_points_movebank_ECMWF_calc ON (guillemots_gps_points_igu.device_info_serial = guillemots_gps_points_movebank_ECMWF_calc.device_info_serial) AND (guillemots_gps_points_igu.date_time = guillemots_gps_points_movebank_ECMWF_calc.date_time)) INNER JOIN guillemots_gps_points_components_wind ON (guillemots_gps_points_movebank_ECMWF_calc.device_info_serial = guillemots_gps_points_components_wind.device_info_serial) AND (guillemots_gps_points_movebank_ECMWF_calc.date_time = guillemots_gps_points_components_wind.date_time)) INNER JOIN guillemots_gps_points_movebank_ecmwf ON (guillemots_gps_points_components_wind.device_info_serial = guillemots_gps_points_movebank_ecmwf.device_info_serial) AND (guillemots_gps_points_components_wind.date_time = guillemots_gps_points_movebank_ecmwf.date_time)
                          WHERE (((guillemots_gps_points_igu.date_time)>=[guillemots_track_session].[start_date] And (guillemots_gps_points_igu.date_time)<=[guillemots_track_session].[end_date]) AND ((guillemots_gps_points_igu.latitude)<>0) AND ((guillemots_gps_points_igu.longitude)<>0) AND ((guillemots_track_session.device_info_serial)=[guillemots_gps_points_igu].[device_info_serial]))
                          ORDER BY guillemots_gps_points_igu.device_info_serial, guillemots_gps_points_igu.date_time;
                          ",
                          as.is = TRUE)


# Columns are the same, but name convention differs - give same names
# so that row bind works
names(gpspoints.uva) <- names(gpspoints.igu)


# Combine
gpspoints <- rbind(gpspoints.uva, gpspoints.igu)

# Get data into correct types
str(gpspoints)
# datetime
gpspoints$date_time <- as.POSIXct(gpspoints$date_time, tz = "UTC")



# Check for and remove duplicates
test <- cbind(gpspoints$device_info_serial, gpspoints$date_time)
test2 <- duplicated(test)
summary(test2)
# no duplicates
gpspoints <- gpspoints[!test2,]




summary(as.factor(gpspoints$trip_id))

summary((gpspoints$trip_id) == 0)

gpspoints$years <- format(gpspoints$date_time, "%Y")
gpspoints$years <- (as.factor(gpspoints$years))

f2009 <- gpspoints$years == "2009" & gpspoints$trip_id != 0
summary(f2009)

f2014 <- gpspoints$years == "2014" & gpspoints$trip_id != 0
summary(f2014)

f2015 <- gpspoints$years == "2015" & gpspoints$trip_id != 0
summary(f2015)

# ***** 1. Maps for 3 years of foraging tracks -------


# Alpha channel ----
addalpha <- function(colors, alpha=1.0) {
  r <- col2rgb(colors, alpha=T)
  # Apply alpha
  r[4,] <- alpha*255
  r <- r/255.0
  return(rgb(r[1,], r[2,], r[3,], r[4,]))
}

# Function to plot a base map ----
# Plot a map with coastlines only - no 
library(maps)
library(RColorBrewer)
# install.packages("colorspace")
library(colorspace)


# Coastline data
load("SWE_adm0.RData")


map.base.fun <- function(xlim = c(17,18.3), ylim =  c(56.8,57.7),
                         box.col = "black", box.lwd = 2){
  par(mfrow=c(1,1))
  par( mar = c(5, 4, 4, 2))
  #   par(bg = 'white')
  
  # col.green <- brewer.pal(5,"Greens")
  # col.blue <- brewer.pal(5,"Blues")
  
  
  plot(gadm, xlim = xlim,
       ylim = ylim, col= "grey40", bg = "white",
       lty = 0)
  
  ## Scale bar and axis
  # box(col= box.col,lwd= box.lwd)
  axis(side=(1),las=1,col="white",col.axis="white",col.ticks = "white")
  axis(side=(2),las=1,col="white",col.axis="white",col.ticks = "white")
  box(col= box.col,lwd= box.lwd)
  
}



# 
# 
# str(gpspoints)
# plot(gpspoints$latitude[f2009]~gpspoints$longitude[f2009])
# 
# 
# plot(gpspoints$latitude[f2015]~gpspoints$longitude[f2015])
# 
# plot(gpspoints$latitude[f2014]~gpspoints$longitude[f2014])
# 


# * 2009 -----

# lbbg_flight_ids <- unique(gps_lbbg$flight_id)
# col.vec <- rainbow(length(land_10))
birds <- unique(gpspoints$ring_number[f2009])
b <- length(birds)
col.vec <- rainbow_hcl(b)
# ?rainbow_hcl
# ?rainbow

col.vec.al <- addalpha(col.vec, alpha = 0.7)
col.vec.al.rand <- col.vec.al[sample(seq_along(col.vec.al))]

resa = 300
# png("wsc_2009_trips.png", res = resa, width = 8*resa, height = 8*resa)
pdf("wsc_2009_trips.pdf", width = 8, height = 8)



map.base.fun(box.col = rainbow_hcl(3)[1],
             box.lwd = 6)

for(ib in 1:b){
  
  trips <- unique(gpspoints$trip_id[f2009 & gpspoints$ring_number == birds[ib]])
  t <- length(trips)
  
  for(i in 1:t){
    
    x <- trips[i]
    # ?subset
    gps.sub <- gpspoints[f2009 & gpspoints$trip_id == x &
                           gpspoints$ring_number == birds[ib],
                         c("longitude", "latitude")]
    n <- length(gps.sub$longitude)
    segments(gps.sub$longitude[-1], gps.sub$latitude[-1],
             gps.sub$longitude[1:n-1], gps.sub$latitude[1:n-1],
             col = col.vec.al.rand[ib], lty = 1, lwd = 2)
  }
}

map.scale(ratio = FALSE,
          relwidth = 0.25, cex = 1.2)
dev.off()




# * 2015 -----

# lbbg_flight_ids <- unique(gps_lbbg$flight_id)
# col.vec <- rainbow(length(land_10))
birds <- unique(gpspoints$ring_number[f2015])
b <- length(birds)
col.vec <- rainbow_hcl(b)
# ?rainbow_hcl
# ?rainbow

col.vec.al <- addalpha(col.vec, alpha = 0.7)
col.vec.al.rand <- col.vec.al[sample(seq_along(col.vec.al))]

resa = 300
png("wsc_2015_trips.png", res = resa, width = 8*resa, height = 8*resa)

pdf("wsc_2015_trips.pdf", width = 8, height = 8)
# svg("wsc_2015_trips.svg", width = 8, height = 8)
# ?pdf

map.base.fun(box.col = rainbow_hcl(3)[2],
             box.lwd = 6)
for(ib in 1:b){
  
  trips <- unique(gpspoints$trip_id[f2015 & gpspoints$ring_number == birds[ib]])
  t <- length(trips)
  
  for(i in 1:t){
    
    x <- trips[i]
    # ?subset
    gps.sub <- gpspoints[f2015 & gpspoints$trip_id == x &
                           gpspoints$ring_number == birds[ib],
                         c("longitude", "latitude")]
    n <- length(gps.sub$longitude)
    segments(gps.sub$longitude[-1], gps.sub$latitude[-1],
             gps.sub$longitude[1:n-1], gps.sub$latitude[1:n-1],
             col = col.vec.al.rand[ib], lty = 1, lwd = 2)
  }
}

map.scale(ratio = FALSE,
          relwidth = 0.25, cex = 1.2)
dev.off()




# * 2014 -----

# lbbg_flight_ids <- unique(gps_lbbg$flight_id)
# col.vec <- rainbow(length(land_10))
birds <- unique(gpspoints$ring_number[f2014])
b <- length(birds)
col.vec <- rainbow_hcl(b)
# ?rainbow_hcl
# ?rainbow

col.vec.al <- addalpha(col.vec, alpha = 0.7)
col.vec.al.rand <- col.vec.al[sample(seq_along(col.vec.al))]

resa = 300
png("wsc_2014_trips_trans.png", res = resa, width = 8*resa, height = 8*resa,
    bg = "transparent")

pdf("wsc_2014_trips.pdf", width = 8, height = 8)

# win.metafile(filename = "", width = 7, height = 7, pointsize = 12)
# ?png

# ?pdf

map.base.fun(box.col = rainbow_hcl(3)[3],
             box.lwd = 6)
for(ib in 1:b){
  
  trips <- unique(gpspoints$trip_id[f2014 & gpspoints$ring_number == birds[ib]])
  t <- length(trips)
  
  for(i in 1:t){
    
    x <- trips[i]
    # ?subset
    gps.sub <- gpspoints[f2014 & gpspoints$trip_id == x &
                           gpspoints$ring_number == birds[ib],
                         c("longitude", "latitude")]
    n <- length(gps.sub$longitude)
    segments(gps.sub$longitude[-1], gps.sub$latitude[-1],
             gps.sub$longitude[1:n-1], gps.sub$latitude[1:n-1],
             col = col.vec.al.rand[ib], lty = 1, lwd = 2)
  }
}

map.scale(ratio = FALSE,
          relwidth = 0.25, cex = 1.2)
dev.off()




# Location map ------
# Region map
library("mapdata")
pdf("wsc_regional_location.pdf", width = 4, height = 4)
# ?png
map('worldHires', xlim = c(-5,30), ylim = c(50,65),
    col= "grey40", bg = "white",
    fill = TRUE,lty = 0)
# ?polygons
# c(17.1, 18.2), ylim = c(56.8, 57.7)
rect(16.8, 56.8, 18.6, 57.7, density = FALSE, angle = 45,
     col = "red", border = "red", lwd = 3)
## Scale bar and axis
box(col="dark grey",lwd=4)
# axis(side=(1),las=1,col="dark grey",col.axis="dark grey")
# axis(side=(2),las=1,col="dark grey",col.axis="dark grey")
# ?map
dev.off()



# ******* 2. Breeding success etc ----
# chick weight plots ------
ch_weight <- read.csv(file = "MurreChickWeights98-15.csv", header = TRUE)

library(ggplot2)
library(grid)

# install.packages("ggplot2")

# pdf("wsc_chick_weight.pdf",  width = 8, height = 8)
cairo_ps("wsc_chick_weight_new.ps",  width = 8, height = 8)
p <- ggplot(ch_weight, aes(Yr, chick_wt))
p +   # scale_x_continuous(limits = c(2005,2015))+
  geom_smooth( lwd = 1, col = "grey60") +
  geom_vline(xintercept = 2009, color = rainbow_hcl(3)[1],
             linetype = "longdash", size = 2) +
  geom_vline(xintercept = 2014, color = rainbow_hcl(3)[3],
             linetype = "longdash", size = 2) +
  geom_vline(xintercept = 2015, color = rainbow_hcl(3)[2],
             linetype = "longdash", size = 2) +
  geom_point(
    ylim = c(210, 255),
    xlim = c(1988,2015),
    size = I(4)) +
  labs(x = "Year", y = "Chick weight (g)") +
  theme( axis.text  = element_text(size=16),
         axis.title = element_text(size=20)) +
  scale_x_continuous(breaks=c(seq(2005,2015,2)),
                     limits = c(2005,2015)) +
  scale_y_continuous(breaks=c(seq(220,255,5)),
                     limits = c(215,257)) +
  theme(axis.text=element_text(size=25),
        axis.title=element_text(size=30,face="bold")) +
  theme(axis.title.x=element_text(vjust=-1)) +
  theme(axis.title.y=element_text(angle=90, vjust=2)) +
  theme(plot.title=element_text(size=15, vjust=3)) +
  theme(plot.margin = unit(c(1,1,1,1), "cm"))
# ?labs
# ?geom_smooth
dev.off()
# ?seq

# ggsave(filename = "test.pdf", plot = p2, width = 6, height = 6)
# ?ggsave




cairo_ps("wsc_chick_fledge_new.ps",  width = 8, height = 8)

# pdf("wsc_chick_fledge.pdf",  width = 8, height = 8)
p <- ggplot(ch_weight, aes(Yr, fledg_suc))
p +   # scale_x_continuous(limits = c(2005,2015))+
  geom_smooth( lwd = 1, col = "grey60") +
  geom_vline(xintercept = 2009, color = rainbow_hcl(3)[1],
             linetype = "longdash", size = 2) +
  geom_vline(xintercept = 2014, color = rainbow_hcl(3)[3],
             linetype = "longdash", size = 2) +
  geom_vline(xintercept = 2015, color = rainbow_hcl(3)[2],
             linetype = "longdash", size = 2) +
  geom_point(
    ylim = c(60, 90),
    xlim = c(1988,2015),
    size = I(4)) +
  labs(x = "Year", y = "Breeding success (%)") +
  theme( axis.text  = element_text(size=16),
         axis.title = element_text(size=20)) +
  scale_x_continuous(breaks=c(seq(2005,2015,2)),
                     limits = c(2005,2015)) +
  scale_y_continuous(breaks=c(seq(60,90,5)),
                     limits = c(60,90)) +
  # theme(axis.text=element_text(size=18),
        # axis.title=element_text(size=20,face="bold")) +
  theme(axis.text=element_text(size=25),
        axis.title=element_text(size=30,face="bold")) +
  
  theme(axis.title.x=element_text(vjust=-1)) +
  theme(axis.title.y=element_text(angle=90, vjust=2)) +
  theme(plot.title=element_text(size=15, vjust=3)) +
  theme(plot.margin = unit(c(1,1,1,1), "cm"))
# ?labs
# ?geom_smooth
dev.off()
# ?seq



# ******** Weighting criterion for GPS locations --------

# Add column for time interval (note will be wrong between tags - but go with it anyway...)
# ?difftime

n <- length(gpspoints$date_time)
t.interval <- difftime(gpspoints$date_time[-1], gpspoints$date_time[-n])
hist(as.numeric(t.interval), xlim = c(-1000,1000))

t.interval <- as.numeric(t.interval)
t.interval[t.interval < 0] <- NA
t.interval[t.interval > 800] <- NA

hist(t.interval)
t.interval <- c(NA, t.interval)

gpspoints$interval <- t.interval


# By year

# 2009

gps.sub <- gpspoints[f2009,]

birds <- unique(gps.sub$ring_number)
b <- length(birds)

gps.sub$weight <- NA

# str(gps.sub)

for(i in 1:b){
  fb <- gps.sub$ring_number == birds[i]
  t.tot <- sum(gps.sub$interval[fb], na.rm = TRUE)
  gps.sub$weight[fb] <- gps.sub$interval[fb]/t.tot
  
}


gps.all.dist <- gps.sub


# 2014
gps.sub <- gpspoints[f2014,]

birds <- unique(gps.sub$ring_number)
b <- length(birds)

gps.sub$weight <- NA

# str(gps.sub)

for(i in 1:b){
  fb <- gps.sub$ring_number == birds[i]
  t.tot <- sum(gps.sub$interval[fb], na.rm = TRUE)
  gps.sub$weight[fb] <- gps.sub$interval[fb]/t.tot
  
}


gps.all.dist <- rbind.data.frame(gps.all.dist,gps.sub)


# 2015
gps.sub <- gpspoints[f2015,]

birds <- unique(gps.sub$ring_number)
b <- length(birds)

gps.sub$weight <- NA

# str(gps.sub)

for(i in 1:b){
  fb <- gps.sub$ring_number == birds[i]
  t.tot <- sum(gps.sub$interval[fb], na.rm = TRUE)
  gps.sub$weight[fb] <- gps.sub$interval[fb]/t.tot
  
}


gps.all.dist <- rbind.data.frame(gps.all.dist,gps.sub)








# hist(gps.sub$ground_speed)

gps.all.dist.new <- gps.all.dist[gps.all.dist$ground_speed < 5 & gps.all.dist$coldist > 1500 &
                                   !is.na(gps.all.dist$weight),]


# Weight years
# tot09 <- sum(gps.all.dist.new$weight[gps.all.dist.new$years == 2009])
# gps.all.dist.new$weight[gps.all.dist.new$years == 2009] <- gps.all.dist.new$weight[gps.all.dist.new$years == 2009]/tot09
# tot14 <- sum(gps.all.dist.new$weight[gps.all.dist.new$years == 2014])
# gps.all.dist.new$weight[gps.all.dist.new$years == 2014] <- gps.all.dist.new$weight[gps.all.dist.new$years == 2014]/tot14
# tot15 <- sum(gps.all.dist.new$weight[gps.all.dist.new$years == 2015])
# gps.all.dist.new$weight[gps.all.dist.new$years == 2015] <- gps.all.dist.new$weight[gps.all.dist.new$years == 2015]/tot15


gps.all.dist.new$years <- factor(gps.all.dist.new$years,levels(gps.all.dist.new$years)[c(4,3,2,1)])

# ?geom_density

# *** col dist ------

# pdf("wsc_coldist.pdf",  width = 8, height = 8)
cairo_ps("wsc_coldist2_new.ps",  width = 12, height = 8)

ggplot(gps.all.dist.new, aes(x = coldist/1000, weight = weight/sum(weight),
                             fill = factor(years), y = ..scaled..
)) +
  geom_density(alpha = 0.6) +
  scale_fill_manual(values=rainbow_hcl(3)[c(2,3,1)])+
  # scale_colour_manual(values=c("#3182bd", "#c51b8a"))+
  # scale_x_continuous(limits = c(-10, 10))+
  # scale_y_continuous(limits = c(-25, 100)) +
  theme(legend.position="none") +
  theme(axis.text=element_text(size=18),
        axis.title=element_text(size=20,face="bold")) +
  labs(x = "Distance from colony (km)",
       y = "Relative frequency") +
  # theme(axis.text=element_text(size=18),
        # axis.title=element_text(size=20,face="bold")) +
  theme(axis.text=element_text(size=25),
        axis.title=element_text(size=30,face="bold")) +
  
  theme(axis.title.x=element_text(vjust=-1)) +
  theme(axis.title.y=element_text(angle=90, vjust=2)) +
  theme(plot.title=element_text(size=15, vjust=3)) +
  theme(plot.margin = unit(c(1,1,1,1), "cm"))
dev.off()







# Bathymetry data -------
library("raster")
bath_raster <- raster("bsbd-0.9.3.grd")
# install.packages("rgdal")
# install.packages("ncdf4", type = "source")
# Difficult to install for some reason - in the end installed from
# http://cirrus.ucsd.edu/~pierce/ncdf/
# ?install.packages
xy.gps <- cbind(gps.all.dist.new$longitude, gps.all.dist.new$latitude)

# Extract bathymetry data for these positions
gps.bath <- extract(bath_raster,xy.gps)

gps.all.dist.new$bath <- gps.bath

hist(gps.bath)

# summary(is.na(gps.all.dist.new$bath))

# pdf("wsc_coldist.pdf",  width = 8, height = 8)
cairo_ps("wsc_depth2_new.ps",  width = 6, height = 8)

ggplot(gps.all.dist.new, aes(x = bath, weight = weight/sum(weight),
                             fill = factor(years), y = ..scaled..
)) +
  geom_density(alpha = 0.6) +
  scale_fill_manual(values=rainbow_hcl(3)[c(2,3,1)])+
  # scale_colour_manual(values=c("#3182bd", "#c51b8a"))+
  # scale_x_continuous(limits = c(-10, 10))+
  # scale_y_continuous(limits = c(-25, 100)) +
  theme(legend.position="none") +
  theme(axis.text=element_text(size=18),
        axis.title=element_text(size=20,face="bold")) +
  labs(x = "Water depth (m)",
       y = "Relative frequency") +
  # theme(axis.text=element_text(size=18),
        # axis.title=element_text(size=20,face="bold")) +
  theme(axis.text=element_text(size=25),
        axis.title=element_text(size=30,face="bold")) +
  
    theme(axis.title.x=element_text(vjust=-1)) +
  theme(axis.title.y=element_text(angle=90, vjust=2)) +
  theme(plot.title=element_text(size=15, vjust=3)) +
  theme(plot.margin = unit(c(1,1,1,1), "cm")) +
  coord_flip()
dev.off()


unique(cbind.data.frame(gps.all.dist.new$ring_number,
                        gps.all.dist.new$years))



# Mapping bathymetry ------

pal <- choose_palette()

?cairo_ps

cairo_ps("wsc_bath2.ps",  width = 7, height = 7,  bg = NA)
# png("test_bath.png")
  par(mfrow=c(1,1))
  par( mar = c(5, 4, 4, 5))
  par(col.sub = "white",
      col.main = "white",
      col.lab = "white",
      col.axis = "white")

  
  cxlim <- c(17,18.2) 
  cylim <-  c(56.9,57.5)
  
  plot(gadm, xlim = cxlim,
       ylim = cylim, col= "grey40", bg = NA,
       lty = 0, add = FALSE)
  plot(bath_raster, xlim = c(16,19),
       ylim = c(56.5,58), col= pal(50), bg = NA,
       lty = 0, add = TRUE)
  plot(gadm, xlim = xlim,
       ylim = ylim, col= "grey40", bg = NA,
       lty = 0, add = TRUE)
  
  
  axis(side=(1),las=1,col="white",col.axis="white",col.ticks = "white")
  axis(side=(2),las=1,col="white",col.axis="white",col.ticks = "white")
  box(col= box.col,lwd= box.lwd)
  axis(side=(1),las=1,col="white",col.axis="white",col.ticks = "white")
  axis(side=(2),las=1,col="white",col.axis="white",col.ticks = "white")
  box.col <- "white"
  box.lwd <- 6
  box(col= box.col,lwd= box.lwd)
  
  dev.off()
  
  
  
  
col.pal <- function (n, h = c(300, 200), c. = c(60, 0), l = c(25, 95), power = c(0.7, 
                                                                                 1.3), fixup = TRUE, gamma = NULL, alpha = 1, ...) {
  if (!is.null(gamma)) 
    warning("'gamma' is deprecated and has no effect")
  if (n < 1L) 
    return(character(0L))
  h <- rep(h, length.out = 2L)
  c <- rep(c., length.out = 2L)
  l <- rep(l, length.out = 2L)
  power <- rep(power, length.out = 2L)
  rval <- seq(1, 0, length = n)
  rval <- hex(polarLUV(L = l[2L] - diff(l) * rval^power[2L], 
                       C = c[2L] - diff(c) * rval^power[1L], H = h[2L] - diff(h) * 
                         rval), fixup = fixup, ...)
  if (!missing(alpha)) {
    alpha <- pmax(pmin(alpha, 1), 0)
    alpha <- format(as.hexmode(round(alpha * 255 + 1e-04)), 
                    width = 2L, upper.case = TRUE)
    rval <- paste(rval, alpha, sep = "")
  }
  return(rval)
}  





# ************ Dive depth data ---------
  
# Import 2009 data ----

files <- list.files(path = 
                      "D:/Dropbox/Guillemots/guillemot_2009_data/guillemot_2009_dive_data/2009_depth",
                    pattern = ".TXT",
                    all.files = FALSE,
                    full.names = FALSE, recursive = FALSE,
                    ignore.case = FALSE, include.dirs = FALSE)
files
# Vector of device IDs
fun.wrap <- function(x){
  # strsplit(x, split = ".txt" )[[1]][1]
  substr(x, 1, 6)
}

ring_number_09 <- sapply(X = files, FUN = fun.wrap)
names(ring_number_09) <- NULL

# 
# # Source functions
# source("parse_igotu2gpx_txt.R")

# Parse all files
n <- length(files)

parse.list <- list()
i <- 1
for(i in 1:n){
  temp <- read.table(paste("D:/Dropbox/Guillemots/guillemot_2009_data/guillemot_2009_dive_data/2009_depth/",
    files[i], sep = ""), sep = "\t")
  x <- cbind(temp,ring_number_09[i],files[i],"2009")
  parse.list[[i]] <- x
}

# Combine all into single data frame
points.df <- do.call("rbind", parse.list)
str(points.df)
names(points.df) <- c("date_time", "depth_db", "ring_number", "file_name", "year")

str(points.df)


t <- strptime(as.character(points.df$date_time), format = "%d/%m/%y %H:%M:%S", tz = "UTC")
points.df$date_time <- as.POSIXct(t , tz = "UTC")

points.df$depth_db[points.df$ring_number == "AAK959"] <- 
  points.df$depth_db[points.df$ring_number == "AAK959"] - 8.85

# 
hist(points.df$depth_db)
f <- points.df$depth_db >5
hist(points.df$depth_db[f], breaks = 100, xlim = c(0,100))
# 
# f <- points.df$depth_db >8   &   points.df$depth_db < 10
# points.df$depth_db[f]
# 
# summary(points.df$ring_number[points.df$depth_db > 45  &
#                                 points.df$depth_db < 55])
# 
# 
f <- points.df$depth_db >5   &  points.df$ring_number != "AAK963"
#   points.df$ring_number != "AAK963"
hist(points.df$depth_db[f], breaks = 100, xlim = c(0,100))
# 
# 
# points.aak963 <- points.df[points.df$ring_number == "AAK959",]
# hist(points.aak963$depth_db,  breaks = 100, xlim = c(0,90), ylim = c(0,40000))
# hist(points.aak963$depth_db,  breaks = 100, xlim = c(5,40), ylim = c(0,40000))
# hist(points.aak963$depth_db[1:100000], breaks = 100, xlim = c(5,40), ylim = c(0,40000))
# hist(points.aak963$depth_db[1:200000], breaks = 100, xlim = c(5,40), ylim = c(0,40000))
# hist(points.aak963$depth_db[200000:300000], breaks = 100, xlim = c(5,40), ylim = c(0,10000))
# hist(points.aak963$depth_db[400000:408900], breaks = 100, xlim = c(5,40), ylim = c(0,1000))
# min(points.aak963$depth_db)



# Import 2015 data ------

files <- list.files(path = 
                      "D:/Dropbox/Guillemots/2015/2015_depth/2015_depth_edit/",
                    pattern = ".csv",
                    all.files = FALSE,
                    full.names = FALSE, recursive = FALSE,
                    ignore.case = FALSE, include.dirs = FALSE)
files
# Vector of device IDs
fun.wrap <- function(x){
  # strsplit(x, split = ".txt" )[[1]][1]
  substr(x, 27, 32)
}

ring_number_15 <- sapply(X = files, FUN = fun.wrap)
names(ring_number_15) <- NULL
ring_number_15 <- toupper(ring_number_15)
# 
# # Source functions
# source("parse_igotu2gpx_txt.R")

# Parse all files
n <- length(files)

parse.list <- list()
i <- 1
for(i in 1:n){
  temp <- read.table(paste("D:/Dropbox/Guillemots/2015/2015_depth/2015_depth_edit/",
                           files[i], sep = ""), sep = ",",
                     skip = 3)
  x <- cbind(temp,ring_number_15[i],files[i],"2015")
  parse.list[[i]] <- x
}

# Combine all into single data frame
points.df.2015 <- do.call("rbind", parse.list)
str(points.df.2015)



names(points.df.2015) <- c("idx", "date", "time", "depth_db",
                           "temp_c", "wet_dry", "ring_number",
                           "file_name", "year")

str(points.df.2015)

t_comb <- paste(points.df.2015$date, points.df.2015$time, sep = " ")
str(t_comb)
# t <- strptime((t_comb), format = "%d/%m/%y %H:%M:%S", tz = "UTC")
date_time_2015 <- as.POSIXct(t_comb , tz = "UTC")
# head(test2)
points.df.2015.new <- cbind.data.frame(date_time_2015,
                                       points.df.2015$depth_db,
                                       points.df.2015$ring_number,
                                       points.df.2015$file_name,
                                       points.df.2015$year)

# hist(points.df.2015.new$points.df.2015$depth_db)
names(points.df.2015.new) <- names(points.df)

hist(points.df.2015.new$depth_db, xlim = c(-10,100), ylim = c(0,10000), breaks = 100)
hist(points.df.2015.new$depth_db, xlim = c(-10,100), ylim = c(0,20000), breaks = 100)
hist(points.df.2015.new$depth_db, xlim = c(39,49), ylim = c(0,5000), breaks = 1000)




# Combine the two years -------
dive_all <- rbind(points.df, points.df.2015.new)

summary(dive_all$ring_number)
summary(points.df$ring_number)
summary(points.df.2015.new$ring_number)

# Do weighting thing ----





n <- length(dive_all$date_time)
t.interval <- difftime(dive_all$date_time[-1], dive_all$date_time[-n])
# hist(as.numeric(t.interval), xlim = c(-1000,1000))

t.interval <- as.numeric(t.interval)
t.interval[t.interval < 0] <- NA
t.interval[t.interval > 100] <- NA

hist(t.interval)
t.interval <- c(NA, t.interval)

dive_all$interval <- t.interval


# By year

# 2009

dive_all.sub <- dive_all[dive_all$year == "2009",]

birds <- unique(dive_all.sub$ring_number)
b <- length(birds)

dive_all.sub$weight <- NA

# str(gps.sub)
i <- 1
for(i in 1:b){
  fb <- dive_all.sub$ring_number == birds[i]
  t.tot <- sum(dive_all.sub$interval[fb], na.rm = TRUE)
  dive_all.sub$weight[fb] <- dive_all.sub$interval[fb]/t.tot
  
}


dive_all.new <- dive_all.sub


# 2015
# str(dive_all$year)
dive_all.sub <- dive_all[dive_all$year == "2015",]

birds <- unique(dive_all.sub$ring_number)
b <- length(birds)

dive_all.sub$weight <- NA

# str(gps.sub)
i <- 1
for(i in 1:b){
  fb <- dive_all.sub$ring_number == birds[i]
  t.tot <- sum(dive_all.sub$interval[fb], na.rm = TRUE)
  dive_all.sub$weight[fb] <- dive_all.sub$interval[fb]/t.tot
  
}
# summary(fb)
dive_all.new <- rbind.data.frame(dive_all.new,dive_all.sub)

# x <- dive_all.sub$ring_number[is.na(fb)]
# head(x)

hist(dive_all.new$depth_db[dive_all.new$depth_db > 5])
# max(dive_all.new$depth_db[dive_all.new$depth_db > 5])
# Filter data ----
dive_all.new_f <- dive_all.new[dive_all.new$ring_number != "AAK963" &
                                   !is.na(dive_all.new$weight) &
                                   dive_all.new$depth_db > 5
                                 ,]

# dive_all.new_f$years <- factor(gps.all.dist.new$years,levels(gps.all.dist.new$years)[c(4,3,2,1)])

str(dive_all.new_f)
# Plot dive depth data -------
cairo_ps("wsc_depth_dives2_new.ps",  width = 6, height = 8)

ggplot(dive_all.new_f, aes(x = -depth_db, weight = weight/sum(weight),
                             fill = factor(year), y = ..scaled..
)) +
  geom_density(alpha = 0.6) +
  scale_fill_manual(values=rainbow_hcl(3)[c(1,2)])+
  # scale_colour_manual(values=c("#3182bd", "#c51b8a"))+
  # scale_x_continuous(limits = c(-10, 10))+
  # scale_y_continuous(limits = c(-25, 100)) +
  theme(legend.position="none") +
  theme(axis.text=element_text(size=18),
        axis.title=element_text(size=20,face="bold")) +
  labs(x = "Time at depth (m)",
       y = "Relative frequency") +
  # theme(axis.text=element_text(size=18),
        # axis.title=element_text(size=20,face="bold")) +
  theme(axis.text=element_text(size=25),
        axis.title=element_text(size=30,face="bold")) +
    theme(axis.title.x=element_text(vjust=-1)) +
  theme(axis.title.y=element_text(angle=90, vjust=2)) +
  theme(plot.title=element_text(size=15, vjust=3)) +
  theme(plot.margin = unit(c(1,1,1,1), "cm")) +
  scale_x_continuous(breaks=c(seq(-100,0, 20)),
                     limits = c(-100,0)) +
  coord_flip()
dev.off()


all_rec <- cbind.data.frame(dive_all.new_f$ring_number,dive_all.new_f$year)
unique(all_rec)
