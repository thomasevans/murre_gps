
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
# png("wsc_2015_trips.png", res = resa, width = 8*resa, height = 8*resa)

pdf("wsc_2015_trips.pdf", width = 8, height = 8)

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
# png("wsc_2014_trips.png", res = resa, width = 8*resa, height = 8*resa)

pdf("wsc_2014_trips.pdf", width = 8, height = 8)

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

pdf("wsc_chick_weight.pdf",  width = 8, height = 8)
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
  theme(axis.text=element_text(size=18),
        axis.title=element_text(size=20,face="bold")) +
  theme(axis.title.x=element_text(vjust=-1)) +
  theme(axis.title.y=element_text(angle=90, vjust=2)) +
  theme(plot.title=element_text(size=15, vjust=3)) +
  theme(plot.margin = unit(c(1,1,1,1), "cm"))
# ?labs
# ?geom_smooth
dev.off()
# ?seq






pdf("wsc_chick_fledge.pdf",  width = 8, height = 8)
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
  theme(axis.text=element_text(size=18),
        axis.title=element_text(size=20,face="bold")) +
  theme(axis.title.x=element_text(vjust=-1)) +
  theme(axis.title.y=element_text(angle=90, vjust=2)) +
  theme(plot.title=element_text(size=15, vjust=3)) +
  theme(plot.margin = unit(c(1,1,1,1), "cm"))
# ?labs
# ?geom_smooth
dev.off()
# ?seq



# Weighting criterion for GPS locations --------

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
tot09 <- sum(gps.all.dist.new$weight[gps.all.dist.new$years == 2009])
gps.all.dist.new$weight[gps.all.dist.new$years == 2009] <- gps.all.dist.new$weight[gps.all.dist.new$years == 2009]/tot09
tot14 <- sum(gps.all.dist.new$weight[gps.all.dist.new$years == 2014])
gps.all.dist.new$weight[gps.all.dist.new$years == 2014] <- gps.all.dist.new$weight[gps.all.dist.new$years == 2014]/tot14
tot15 <- sum(gps.all.dist.new$weight[gps.all.dist.new$years == 2015])
gps.all.dist.new$weight[gps.all.dist.new$years == 2015] <- gps.all.dist.new$weight[gps.all.dist.new$years == 2015]/tot15


gps.all.dist.new$years <- factor(gps.all.dist.new$years,levels(gps.all.dist.new$years)[c(4,3,2,1)])

# ?geom_density

pdf("wsc_coldist.pdf",  width = 8, height = 8)
ggplot(gps.all.dist.new, aes(x=coldist/1000, weight = weight/sum(weight),
                             fill=factor(years), y = ..scaled..
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
       y = "Density (scaled)") +
  theme(axis.text=element_text(size=18),
        axis.title=element_text(size=20,face="bold")) +
  theme(axis.title.x=element_text(vjust=-1)) +
  theme(axis.title.y=element_text(angle=90, vjust=2)) +
  theme(plot.title=element_text(size=15, vjust=3)) +
  theme(plot.margin = unit(c(1,1,1,1), "cm"))
dev.off()