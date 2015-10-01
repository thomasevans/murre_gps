

# Read in DB data -----
# DB package
library("RODBC")

# Connect to DB
gps.db <- odbcConnectAccess2007('D:/Dropbox/tracking_db/murre_db/murre_db.accdb')


# Get uva data
gpspoints.uva <- sqlQuery(gps.db,
                          query = "SELECT uvabits_gps.device_info_serial, uvabits_gps.date_time, uvabits_gps.latitude, uvabits_gps.longitude, guillemots_gps_points_trip_id.trip_id, guillemots_gps_points_trip_id.deploy_id, guillemots_gps_points_flight_id.flight_id, guillemots_gps_points_movebank_ECMWF_calc.wind_dir, guillemots_gps_points_components_wind.ground_speed, guillemots_gps_points_movebank_ECMWF_calc.wind_speed_10m, guillemots_gps_points_components_wind.alpha, guillemots_gps_points_components_wind.wind_side_10, guillemots_gps_points_components_wind.wind_head_tail_10, guillemots_gps_points_components_wind.wind_side, guillemots_gps_points_components_wind.wind_head_tail, guillemots_gps_points_movebank_ecmwf.ecmwf_cloud_cov_tot, guillemots_gps_points_movebank_ecmwf.ecmwf_pressure_sea_lev, guillemots_gps_points_movebank_ecmwf.ecmwf_temp_2m, guillemots_gps_points_components_wind.head_dir_ecmwf, guillemots_gps_points_components_wind.head_speed_ecmwf, guillemots_gps_points_movebank_ECMWF_calc.wind_speed_flt_ht
FROM guillemots_track_session, (((guillemots_gps_points_flight_id INNER JOIN (guillemots_gps_points_trip_id INNER JOIN (guillemots_gps_points_uva_class INNER JOIN uvabits_gps ON (guillemots_gps_points_uva_class.date_time = uvabits_gps.date_time) AND (guillemots_gps_points_uva_class.device_info_serial = uvabits_gps.device_info_serial)) ON (guillemots_gps_points_trip_id.device_info_serial = uvabits_gps.device_info_serial) AND (guillemots_gps_points_trip_id.date_time = uvabits_gps.date_time)) ON (guillemots_gps_points_flight_id.device_info_serial = guillemots_gps_points_trip_id.device_info_serial) AND (guillemots_gps_points_flight_id.date_time = guillemots_gps_points_trip_id.date_time)) INNER JOIN guillemots_gps_points_movebank_ECMWF_calc ON (uvabits_gps.date_time = guillemots_gps_points_movebank_ECMWF_calc.date_time) AND (uvabits_gps.device_info_serial = guillemots_gps_points_movebank_ECMWF_calc.device_info_serial)) INNER JOIN guillemots_gps_points_components_wind ON (guillemots_gps_points_movebank_ECMWF_calc.date_time = guillemots_gps_points_components_wind.date_time) AND (guillemots_gps_points_movebank_ECMWF_calc.device_info_serial = guillemots_gps_points_components_wind.device_info_serial)) INNER JOIN guillemots_gps_points_movebank_ecmwf ON (guillemots_gps_points_components_wind.date_time = guillemots_gps_points_movebank_ecmwf.date_time) AND (guillemots_gps_points_components_wind.device_info_serial = guillemots_gps_points_movebank_ecmwf.device_info_serial)
WHERE (((uvabits_gps.date_time)>=[guillemots_track_session].[start_date] And (uvabits_gps.date_time)<=[guillemots_track_session].[end_date]) AND ((uvabits_gps.latitude)<>0) AND ((uvabits_gps.longitude)<>0) AND ((guillemots_track_session.device_info_serial)=[uvabits_gps].[device_info_serial]))
ORDER BY uvabits_gps.device_info_serial, uvabits_gps.date_time;
",
                          as.is = TRUE)


# Get igu data
gpspoints.igu <- sqlQuery(gps.db,
                          query = "SELECT guillemots_gps_points_igu.device_info_serial, guillemots_gps_points_igu.date_time, guillemots_gps_points_igu.latitude, guillemots_gps_points_igu.longitude, guillemots_gps_points_trip_id.trip_id, guillemots_gps_points_trip_id.deploy_id, guillemots_gps_points_flight_id.flight_id, guillemots_gps_points_movebank_ECMWF_calc.wind_dir, guillemots_gps_points_components_wind.ground_speed, guillemots_gps_points_movebank_ECMWF_calc.wind_speed_10m, guillemots_gps_points_components_wind.alpha, guillemots_gps_points_components_wind.wind_side_10, guillemots_gps_points_components_wind.wind_head_tail_10, guillemots_gps_points_components_wind.wind_side, guillemots_gps_points_components_wind.wind_head_tail, guillemots_gps_points_movebank_ecmwf.ecmwf_cloud_cov_tot, guillemots_gps_points_movebank_ecmwf.ecmwf_pressure_sea_lev, guillemots_gps_points_movebank_ecmwf.ecmwf_temp_2m, guillemots_gps_points_components_wind.head_dir_ecmwf, guillemots_gps_points_components_wind.head_speed_ecmwf, guillemots_gps_points_movebank_ECMWF_calc.wind_speed_flt_ht
FROM guillemots_track_session, (((guillemots_gps_points_flight_id INNER JOIN (guillemots_gps_points_trip_id INNER JOIN (guillemots_gps_points_igu_class INNER JOIN guillemots_gps_points_igu ON (guillemots_gps_points_igu_class.date_time = guillemots_gps_points_igu.date_time) AND (guillemots_gps_points_igu_class.device_info_serial = guillemots_gps_points_igu.device_info_serial)) ON (guillemots_gps_points_trip_id.device_info_serial = guillemots_gps_points_igu.device_info_serial) AND (guillemots_gps_points_trip_id.date_time = guillemots_gps_points_igu.date_time)) ON (guillemots_gps_points_flight_id.device_info_serial = guillemots_gps_points_trip_id.device_info_serial) AND (guillemots_gps_points_flight_id.date_time = guillemots_gps_points_trip_id.date_time)) INNER JOIN guillemots_gps_points_movebank_ECMWF_calc ON (guillemots_gps_points_igu.date_time = guillemots_gps_points_movebank_ECMWF_calc.date_time) AND (guillemots_gps_points_igu.device_info_serial = guillemots_gps_points_movebank_ECMWF_calc.device_info_serial)) INNER JOIN guillemots_gps_points_components_wind ON (guillemots_gps_points_movebank_ECMWF_calc.date_time = guillemots_gps_points_components_wind.date_time) AND (guillemots_gps_points_movebank_ECMWF_calc.device_info_serial = guillemots_gps_points_components_wind.device_info_serial)) INNER JOIN guillemots_gps_points_movebank_ecmwf ON (guillemots_gps_points_components_wind.date_time = guillemots_gps_points_movebank_ecmwf.date_time) AND (guillemots_gps_points_components_wind.device_info_serial = guillemots_gps_points_movebank_ecmwf.device_info_serial)
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




# Assemble table of flights -----
# - flight_id
# - start_time
# - end_time
# - device_id
# - ring_number

flight_ids <- unique(gpspoints$flight_id)[-1]
head(flight_ids)


flight_start <- NULL
flight_id <- NULL
flight_device_info_serial <- NULL
flight_dep_id <- NULL
trip_id <- NULL
device_type <- NULL
va_mean <- va_median <- va_max <- va_min <- NULL
wind_side_mean <- wind_side_median <- NULL
wind_tail_mean <- wind_tail_median <- NULL
wind_side_10_mean <- wind_side_10_median <- NULL
wind_tail_10_mean <- wind_tail_10_median <- NULL
cloud_tot_mean <- NULL
temp_k_mean <- NULL
wind_10m_mean <- NULL
wind_dir_mean <- NULL
wind_10m_flt_ht_mean <- wind_10m_flt_ht_median <- NULL


library(CircStats)

circ.mean.fun <- function(x){
  if(anyNA(x))return(NA)else{
    xt <- rad(points.sub$wind_dir)
    xy <- deg(circ.mean(xt))
    if(is.na(xy))return(NA) else {
      if(xy <0) return(360 + xy) else return(xy)
      }
  }
}



# i <- 24

for(i in 1:length(flight_ids)){
    # for (i in 1:20){
  id <- flight_ids[i]
  points.sub <- subset(gpspoints, gpspoints$flight_id == id)
  n <-length(points.sub$device_info_serial)
  flight_start[i] <- min(points.sub$date_time)
  flight_id[i]    <- id
  flight_device_info_serial[i] <- as.character(
    points.sub$device_info_serial[1])
  flight_dep_id[i] <- as.character(points.sub$deploy_id[1])
  trip_id[i] <- points.sub$trip_id[1]
  a <- substring(as.character(flight_device_info_serial[i]), 1, 1)
  b <- a == "g"
  if(b){ device_type[i] <- "igu" } else device_type[i] <- "uva"
  
  
  # Weather components
  cloud_tot_mean[i] <- mean(points.sub$ecmwf_cloud_cov_tot, na.rm = TRUE)
  temp_k_mean[i] <- mean(points.sub$ecmwf_temp_2m, na.rm = TRUE)
  wind_10m_mean[i] <- mean(points.sub$wind_speed_10m, na.rm = TRUE)
  
  wind_dir_mean[i] <- circ.mean.fun(points.sub$wind_dir)

  
  # For speeds only include that actual flying points, so only include
  # fixes, where the speed is >5 ms-1
  f <- points.sub$ground_speed > 5
  
  # if(sum(f) > 1){
    va_mean[i] <- mean(points.sub$head_speed_ecmwf[f], na.rm = TRUE)
    va_median[i] <- median(points.sub$head_speed_ecmwf[f], na.rm = TRUE)
    va_max[i] <- max(points.sub$head_speed_ecmwf[f], na.rm = TRUE)
    va_min[i] <- min(points.sub$head_speed_ecmwf[f], na.rm = TRUE)
    
    wind_side_mean[i] <- mean(points.sub$wind_side[f], na.rm = TRUE)
    wind_side_median[i] <- median(points.sub$wind_side[f], na.rm = TRUE)
    
    wind_tail_mean[i] <- mean(points.sub$wind_head_tail[f], na.rm = TRUE)
    wind_tail_median[i] <- median(points.sub$wind_head_tail[f], na.rm = TRUE)
    
    wind_side_10_mean[i] <- mean(points.sub$wind_side_10[f], na.rm = TRUE)
    wind_side_10_median[i] <- median(points.sub$wind_side_10[f], na.rm = TRUE)
    
    wind_tail_10_mean[i] <- mean(points.sub$wind_head_tail_10[f], na.rm = TRUE)
    wind_tail_10_median[i] <- median(points.sub$wind_head_tail_10[f], na.rm = TRUE)
    
    
    
    wind_10m_flt_ht_mean[i] <- mean(points.sub$wind_speed_flt_ht[f], na.rm = TRUE)
    wind_10m_flt_ht_median[i] <- median(points.sub$wind_speed_flt_ht[f], na.rm = TRUE)
    
    
    
    
#   } else {
#     va_mean[i] <- NA
#     va_median[i] <- NA
#     va_max[i] <- NA
#     va_min[i] <- NA
#     
#     wind_side_mean[i] <- NA
#     wind_side_median[i] <- NA
#     
#     wind_tail_mean[i] <- NA
#     wind_tail_median[i] <- NA
#     
#     wind_10m_flt_ht_mean[i] <- NA
#     wind_10m_flt_ht_median[i] <- NA
#     
#     
#   }
  
}


# Compile flight table (before adding trip details) -----

# Correct data format
flight_start <- as.POSIXct(flight_start, origin = "1970-01-01",
                           tz = "UTC")






flights.df <- data.frame(flight_id, flight_dep_id,
                         flight_device_info_serial, flight_start,
                         trip_id, device_type,
                         va_mean, va_median, va_max, va_min,
                         wind_side_mean, wind_side_median,
                         wind_tail_mean, wind_tail_median,
                         wind_side_10_mean, wind_side_10_median,
                         wind_tail_10_mean, wind_tail_10_median,
                         cloud_tot_mean,
                         temp_k_mean,
                         wind_10m_mean,
                         wind_dir_mean,
                         wind_10m_flt_ht_mean,
                         wind_10m_flt_ht_median
                         )

str(flights.df)

names(flights.df) <- c("flight_id",  "deploy_id", 
                       "device_info_serial",  "start_time",
                       "trip_id", "device_type",
                       "va_mean", "va_median", "va_max", "va_min",
                       "wind_side_mean", "wind_side_median",
                       "wind_tail_mean", "wind_tail_median",
                       "wind_side_10_mean", "wind_side_10_median",
                       "wind_tail_10_mean", "wind_tail_10_median",
                       "cloud_tot_mean",
                       "temp_k_mean",
                       "wind_10m_mean",
                       "wind_dir_mean",
                       "wind_10m_flt_ht_mean",
                       "wind_10m_flt_ht_median")

# See how the data looks
# hist(flights.df$dist_straight)
# hist(flights.df$alt_median[flights.df$alt_median >-50 & flights.df$alt_median <100])
# hist(flights.df$alt_median[(flights.df$alt_median >-50) & (flights.df$alt_median <100) &
#        (flights.df$device_type == "uva")], breaks = 50 )
# 

flights.df$start_time <- as.POSIXct(flights.df$start_time, origin = "1970-01-01",
                           tz = "UTC")

str(flights.df)



# Output to DB ----
# will be neccessary to edit table in Access after to define data-types and primary keys and provide descriptions for each variable.
sqlSave(gps.db, flights.df,
        tablename = "guillemots_gps_flights_weather_components_3",
        append = FALSE, rownames = FALSE, colnames = FALSE,
        verbose = FALSE, safer = TRUE, addPK = FALSE, fast = TRUE,
        test = FALSE, nastring = NULL,
        varTypes =  c(start_time = "datetime")
)





