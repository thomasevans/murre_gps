

# Script to map both lbbg and murre flights

# Get data -----

# Read in DB data
# DB package
library("RODBC")

# Connect to murre DB
gps.db <- odbcConnectAccess2007('D:/Dropbox/tracking_db/murre_db/murre_db.accdb')

# Establish a connection to the lbbg database
gps.db2 <- odbcConnectAccess2007('D:/Dropbox/tracking_db/GPS_db.accdb')


# Get flight info - murre

# Get GPS data
flights_murre <- sqlQuery(gps.db,
                    query = "SELECT guillemots_gps_flights.*, guillemots_gps_flights_weather_components_2.va_mean, guillemots_gps_flights_weather_components_2.va_median, guillemots_gps_flights_weather_components_2.va_max, guillemots_gps_flights_weather_components_2.va_min, guillemots_gps_flights_weather_components_2.wind_side_mean, guillemots_gps_flights_weather_components_2.wind_side_median, guillemots_gps_flights_weather_components_2.wind_tail_mean, guillemots_gps_flights_weather_components_2.wind_tail_median, guillemots_gps_flights_weather_components_2.cloud_tot_mean, guillemots_gps_flights_weather_components_2.temp_k_mean, guillemots_gps_flights_weather_components_2.wind_10m_mean, guillemots_gps_flights_weather_components_2.wind_dir_mean, guillemots_gps_flights_weather_components_2.wind_10m_flt_ht_mean, guillemots_gps_flights_weather_components_2.wind_10m_flt_ht_median
                    FROM guillemots_gps_flights INNER JOIN guillemots_gps_flights_weather_components_2 ON guillemots_gps_flights.flight_id = guillemots_gps_flights_weather_components_2.flight_id;
                    ",
                    as.is = TRUE)

# Get flight info - lbbg
flights_lbbg <- sqlQuery(gps.db2, query="SELECT DISTINCT f.*
                    FROM lund_flight_com_lbbg AS f
                 ORDER BY f.device_info_serial ASC, f.start_time ASC;")


# Get flight GPS locations - lbbg
gps_lbbg <- sqlQuery(gps.db2, query="SELECT gps_uva_tracking_speed_3d_limited.device_info_serial, gps_uva_tracking_speed_3d_limited.date_time, gps_uva_tracking_speed_3d_limited.latitude, gps_uva_tracking_speed_3d_limited.longitude, lund_flight_com_lbbg.flight_id
FROM gps_uva_tracking_speed_3d_limited, lund_flight_com_lbbg
                     WHERE (((gps_uva_tracking_speed_3d_limited.date_time)>=[lund_flight_com_lbbg].[start_time] And (gps_uva_tracking_speed_3d_limited.date_time)<=[lund_flight_com_lbbg].[end_time]) AND ((gps_uva_tracking_speed_3d_limited.latitude)<>0) AND ((gps_uva_tracking_speed_3d_limited.longitude)<>0) AND ((lund_flight_com_lbbg.device_info_serial)=[gps_uva_tracking_speed_3d_limited].[device_info_serial]))
                     ORDER BY gps_uva_tracking_speed_3d_limited.device_info_serial, gps_uva_tracking_speed_3d_limited.date_time;")



# Get flight GPS locations - murre
gps_murre <- sqlQuery(gps.db,
                          query = "SELECT * FROM 
(SELECT guillemots_gps_points_igu.device_info_serial, guillemots_gps_points_igu.date_time, guillemots_gps_points_igu.latitude, guillemots_gps_points_igu.longitude, guillemots_gps_flights.flight_id, guillemots_gps_flights.deploy_id
                      FROM guillemots_gps_points_igu, guillemots_gps_flights
                      WHERE (((guillemots_gps_points_igu.date_time)>=[guillemots_gps_flights].[start_time] And (guillemots_gps_points_igu.date_time)<=[guillemots_gps_flights].[end_time]) AND ((guillemots_gps_points_igu.latitude)<>0) AND ((guillemots_gps_points_igu.longitude)<>0) AND ((guillemots_gps_flights.device_info_serial)=[guillemots_gps_points_igu].[device_info_serial]))
                      ORDER BY guillemots_gps_points_igu.device_info_serial, guillemots_gps_points_igu.date_time)
                      
                      UNION SELECT * FROM 
                      (SELECT uvabits_gps.device_info_serial, uvabits_gps.date_time, uvabits_gps.latitude, uvabits_gps.longitude, guillemots_gps_flights.flight_id, guillemots_gps_flights.deploy_id
                      FROM uvabits_gps, guillemots_gps_flights
                      WHERE (((uvabits_gps.date_time)>=[guillemots_gps_flights].[start_time] And (uvabits_gps.date_time)<=[guillemots_gps_flights].[end_time]) AND ((uvabits_gps.latitude)<>0) AND ((uvabits_gps.longitude)<>0) AND ((guillemots_gps_flights.device_info_serial)=[uvabits_gps].[device_info_serial]))
                      ORDER BY uvabits_gps.device_info_serial, uvabits_gps.date_time);",
                          as.is = TRUE)




# Filter murre flights -------
f_dist_col <- flights_murre$col_dist_dif < -1000
f_final <- flights_murre$type == "final"
f_va_alt <- !is.na(flights_murre$va_mean) & !is.na(flights_murre$alt_mean)
f_all_expoints <- f_dist_col & f_final & f_va_alt
summary(f_all_expoints)


flights_murre.f <- flights_murre[f_all_expoints,]
gps_murre.f <- gps_murre[gps_murre$flight_id %in% flights_murre.f$flight_id,]



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

# Coastline data
load("SWE_adm0.RData")


map.base.fun <- function(xlim = c(17,18.3), ylim =  c(57,57.7)){
  par(mfrow=c(1,1))
  par( mar = c(5, 4, 4, 2))
  #   par(bg = 'white')
  
  col.green <- brewer.pal(5,"Greens")
  col.blue <- brewer.pal(5,"Blues")
  
  
  plot(gadm, xlim = xlim,
       ylim = ylim, col= col.green[2], bg = col.blue[1],
       lty = 0)
  
  ## Scale bar and axis
  box(col="dark grey",lwd=3)
  axis(side=(1),las=1,col="dark grey",col.axis="dark grey")
  axis(side=(2),las=1,col="dark grey",col.axis="dark grey")
  
}


# hack map.scale function
# map.scale2 <- map.scale
# fix(map.scale2)
source("map.scale2.R")


# Plot lbbg all flights ----


lbbg_flight_ids <- unique(gps_lbbg$flight_id)
col.vec <- rainbow(length(lbbg_flight_ids))

col.vec.al <- addalpha(col.vec, alpha = 0.3)
col.vec.al.rand <- col.vec.al[sample(seq_along(col.vec.al))]


# Plot all LBBG flights
resa = 72*4
png("lbbg_all_large_scale.png", res = resa, width = 8*resa, height = 8*resa)
# ?png
map.base.fun(xlim = c(17.0, 18.3), ylim = c(56.7, 57.6))
# i <- 12
for(i in 1:length(lbbg_flight_ids)){
  
  x <- lbbg_flight_ids[i]
  # ?subset
  gps.sub <- gps_lbbg[gps_lbbg$flight_id == x,]
  n <- length(gps.sub$longitude)
  segments(gps.sub$longitude[-1], gps.sub$latitude[-1],
           gps.sub$longitude[1:n-1], gps.sub$latitude[1:n-1],
           col = col.vec.al.rand[i], lty = 1, lwd = 1)
}
map.scale2(ratio = FALSE, col = "grey40", line.col = "grey40", lwd.line = 2,
           relwidth = 0.25, cex = 1.2)
dev.off()
# ?map.scale

# Plot murre - all flights -----
murre_flight_ids <- unique(gps_murre.f$flight_id)
col.vec <- rainbow(length(murre_flight_ids))

col.vec.al <- addalpha(col.vec, alpha = 0.3)
col.vec.al.rand <- col.vec.al[sample(seq_along(col.vec.al))]



resa = 72*4
png("murre_all_large_scale.png", res = resa, width = 8*resa, height = 8*resa)
# ?png
map.base.fun(xlim = c(17.0, 18.3), ylim = c(56.7, 57.6))

png("murre_all_small_scale.png", res = resa, width = 8*resa, height = 8*resa)
map.base.fun(xlim = c(17.1, 18.2), ylim = c(57, 57.6))

for(i in 1:length(murre_flight_ids)){
  
  x <- murre_flight_ids[i]
  # ?subset
  gps.sub <- gps_murre.f[gps_murre.f$flight_id == x,]
  n <- length(gps.sub$longitude)
  segments(gps.sub$longitude[-1], gps.sub$latitude[-1],
           gps.sub$longitude[1:n-1], gps.sub$latitude[1:n-1],
           col = col.vec.al.rand[i], lty = 1, lwd = 1)
}
map.scale2(ratio = FALSE, col = "grey40", line.col = "grey40", lwd.line = 2,
           relwidth = 0.25, cex = 1.2)
dev.off()

# Sample flights only ------

# 10 lbbg
lbbg.sub <- c(29534,428,16683,16341,35108,16014,32821,36693,37708,2918)


resa = 72*4
png("lbbg_10.png", res = resa, width = 6*resa, height = 8*resa)
# ?png
map.base.fun(xlim = c(17.1, 18.2), ylim = c(56.8, 57.7))
# i <- 12
for(i in 1:10){
  
  x <- lbbg.sub[i]
  # ?subset
  gps.sub <- gps_lbbg[gps_lbbg$flight_id == x,]
  n <- length(gps.sub$longitude)
  segments(gps.sub$longitude[-1], gps.sub$latitude[-1],
           gps.sub$longitude[1:n-1], gps.sub$latitude[1:n-1],
           col = col.vec.al.rand[i], lty = 1, lwd = 3)
}
map.scale2(ratio = FALSE, col = "grey40", line.col = "grey40", lwd.line = 2,
           relwidth = 0.25, cex = 1.2)
dev.off()



# 10 murres
murre.sub <- sample(murre_flight_ids,10)
murre.sub <- c(140,1148,958,643,456,822,833,1194,640,543)

png("murre_10.png", res = resa, width = 8*resa, height = 8*resa)
map.base.fun(xlim = c(17.1, 18.2), ylim = c(56.8, 57.7))

for(i in 1:10){
  
  x <- murre.sub[i]
  # ?subset
  gps.sub <- gps_murre.f[gps_murre.f$flight_id == x,]
  n <- length(gps.sub$longitude)
  segments(gps.sub$longitude[-1], gps.sub$latitude[-1],
           gps.sub$longitude[1:n-1], gps.sub$latitude[1:n-1],
           col = col.vec.al.rand[i], lty = 1, lwd = 3)
}
map.scale2(ratio = FALSE, col = "grey40", line.col = "grey40", lwd.line = 2,
           relwidth = 0.25, cex = 1.2)
dev.off()





# 10 lbbg + 10 murres
png("murre_lbbg_10.png", res = resa, width = 8*resa, height = 8*resa)
map.base.fun(xlim = c(17.1, 18.2), ylim = c(56.8, 57.7))

png("murre_lbbg_10_oblong.png", res = resa, width = 6*resa, height = 8*resa)
map.base.fun(xlim = c(17.1, 18.2), ylim = c(56.8, 57.7))


mag.trans <- addalpha("#c51b8a", alpha = 0.3)
for(i in 1:10){
  
  x <- murre.sub[i]
  # ?subset
  gps.sub <- gps_murre.f[gps_murre.f$flight_id == x,]
  n <- length(gps.sub$longitude)
  segments(gps.sub$longitude[-1], gps.sub$latitude[-1],
           gps.sub$longitude[1:n-1], gps.sub$latitude[1:n-1],
           col = mag.trans, lty = 1, lwd = 3)
}

for(i in 1:10){
  blu.trans <- addalpha("#3182bd", alpha = 0.3)
  
  x <- lbbg.sub[i]
  # ?subset
  gps.sub <- gps_lbbg[gps_lbbg$flight_id == x,]
  n <- length(gps.sub$longitude)
  segments(gps.sub$longitude[-1], gps.sub$latitude[-1],
           gps.sub$longitude[1:n-1], gps.sub$latitude[1:n-1],
           col = blu.trans, lty = 1, lwd = 3)
}

map.scale2(ratio = FALSE, col = "grey40", line.col = "grey40", lwd.line = 2,
           relwidth = 0.25, cex = 1.2)
dev.off()




# Location maps -----
# World maps

col.green <- brewer.pal(5,"Greens")
col.blue <- brewer.pal(5,"Blues")

png("world_location.png", res = resa, width = 8*resa, height = 8*resa)
# map('world', project = "mollweide", fill = 1, col = col.green[2], bg = col.blue[1])
map('world', project = "globular", fill = 1, col = col.green[2], bg = col.blue[1])
# map('world', project = "gilbert", fill = 1, col = col.green[2], bg = col.blue[1])
points(mapproject(list(y=59, x=20)), col = blu.trans, bg = mag.trans, cex = 4, pch = 21)
dev.off()


# Region map
library("mapdata")
png("regional_location.png", res = resa, width = 8*resa, height = 8*resa)
# ?png
map('worldHires', xlim = c(-5,30), ylim = c(50,65),
    col=col.green[2], bg =  col.blue[1],
    fill = TRUE,lty = 0)
# ?polygons
# c(17.1, 18.2), ylim = c(56.8, 57.7)
rect(17.1, 56.8, 18.2, 57.7, density = NULL, angle = 45,
     col = mag.trans, border = blu.trans, lwd = 3)
## Scale bar and axis
box(col="dark grey",lwd=3)
# axis(side=(1),las=1,col="dark grey",col.axis="dark grey")
# axis(side=(2),las=1,col="dark grey",col.axis="dark grey")
# ?map
dev.off()
