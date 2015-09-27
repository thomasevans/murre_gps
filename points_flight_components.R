


# Read in GPS data including derived wind components, direction (track bearing), etc
# Need:
# lat, long, vg, height, wind stuff, course, device type

# Get the flight data from the db.
library(RODBC)

# Establish a connection to the database
gps.db <- odbcConnectAccess2007('D:/Dropbox/tracking_db/murre_db/murre_db.accdb')



# Get uva data
gpspoints.uva <- sqlQuery(gps.db,
                          query = "SELECT uvabits_gps.device_info_serial, uvabits_gps.date_time, uvabits_gps.latitude, uvabits_gps.longitude, uvabits_gps.altitude, uvabits_gps.speed_2d, guillemots_track_session.ring_number, guillemots_gps_points_trip_id.trip_id, guillemots_gps_points_trip_id.deploy_id, guillemots_gps_points_flight_id.flight_id, guillemots_gps_points_movebank_ecmwf.ecmwf_wind_10m_v, guillemots_gps_points_movebank_ecmwf.ecmwf_wind_10m_u, guillemots_gps_points_movebank_ECMWF_calc.wind_u_10m_flt_ht, guillemots_gps_points_movebank_ECMWF_calc.wind_v_10m_flt_ht, guillemots_gps_points_movebank_ECMWF_calc.wind_speed_flt_ht, guillemots_gps_points_movebank_ECMWF_calc.wind_dir, guillemots_gps_points_movebank_ECMWF_calc.wind_speed_10m, guillemots_track_session.device_type, uvabits_gps.direction
FROM guillemots_track_session, guillemots_gps_points_movebank_ECMWF_calc INNER JOIN (guillemots_gps_points_movebank_ecmwf INNER JOIN (guillemots_gps_points_flight_id INNER JOIN (guillemots_gps_points_trip_id INNER JOIN (guillemots_gps_points_uva_class INNER JOIN uvabits_gps ON (guillemots_gps_points_uva_class.device_info_serial = uvabits_gps.device_info_serial) AND (guillemots_gps_points_uva_class.date_time = uvabits_gps.date_time)) ON (guillemots_gps_points_trip_id.date_time = uvabits_gps.date_time) AND (guillemots_gps_points_trip_id.device_info_serial = uvabits_gps.device_info_serial)) ON (guillemots_gps_points_flight_id.date_time = guillemots_gps_points_trip_id.date_time) AND (guillemots_gps_points_flight_id.device_info_serial = guillemots_gps_points_trip_id.device_info_serial)) ON (guillemots_gps_points_movebank_ecmwf.device_info_serial = guillemots_gps_points_flight_id.device_info_serial) AND (guillemots_gps_points_movebank_ecmwf.date_time = guillemots_gps_points_flight_id.date_time)) ON (guillemots_gps_points_movebank_ECMWF_calc.date_time = uvabits_gps.date_time) AND (guillemots_gps_points_movebank_ECMWF_calc.device_info_serial = uvabits_gps.device_info_serial)
WHERE (((uvabits_gps.date_time)>=[guillemots_track_session].[start_date] And (uvabits_gps.date_time)<=[guillemots_track_session].[end_date]) AND ((guillemots_track_session.device_info_serial)=[uvabits_gps].[device_info_serial]))
ORDER BY uvabits_gps.device_info_serial, uvabits_gps.date_time;
",
                          as.is = TRUE)


# Get igu data
gpspoints.igu <- sqlQuery(gps.db,
                          query = "SELECT guillemots_gps_points_igu.device_info_serial, guillemots_gps_points_igu.date_time, guillemots_gps_points_igu.latitude, guillemots_gps_points_igu.longitude, guillemots_gps_points_igu.elev, guillemots_gps_points_igu.speed_ms, guillemots_track_session.ring_number, guillemots_gps_points_trip_id.trip_id, guillemots_gps_points_trip_id.deploy_id, guillemots_gps_points_flight_id.flight_id, guillemots_gps_points_movebank_ecmwf.ecmwf_wind_10m_v, guillemots_gps_points_movebank_ecmwf.ecmwf_wind_10m_u, guillemots_gps_points_movebank_ECMWF_calc.wind_u_10m_flt_ht, guillemots_gps_points_movebank_ECMWF_calc.wind_v_10m_flt_ht, guillemots_gps_points_movebank_ECMWF_calc.wind_speed_flt_ht, guillemots_gps_points_movebank_ECMWF_calc.wind_dir, guillemots_gps_points_movebank_ECMWF_calc.wind_speed_10m, guillemots_track_session.device_type, guillemots_gps_points_igu.course
FROM guillemots_track_session, guillemots_gps_points_movebank_ECMWF_calc INNER JOIN ((guillemots_gps_points_flight_id INNER JOIN (guillemots_gps_points_trip_id INNER JOIN (guillemots_gps_points_igu_class INNER JOIN guillemots_gps_points_igu ON (guillemots_gps_points_igu_class.device_info_serial = guillemots_gps_points_igu.device_info_serial) AND (guillemots_gps_points_igu_class.date_time = guillemots_gps_points_igu.date_time)) ON (guillemots_gps_points_trip_id.device_info_serial = guillemots_gps_points_igu_class.device_info_serial) AND (guillemots_gps_points_trip_id.date_time = guillemots_gps_points_igu_class.date_time)) ON (guillemots_gps_points_flight_id.device_info_serial = guillemots_gps_points_igu.device_info_serial) AND (guillemots_gps_points_flight_id.date_time = guillemots_gps_points_igu.date_time)) INNER JOIN guillemots_gps_points_movebank_ecmwf ON (guillemots_gps_points_flight_id.device_info_serial = guillemots_gps_points_movebank_ecmwf.device_info_serial) AND (guillemots_gps_points_flight_id.date_time = guillemots_gps_points_movebank_ecmwf.date_time)) ON (guillemots_gps_points_movebank_ECMWF_calc.date_time = guillemots_gps_points_movebank_ecmwf.date_time) AND (guillemots_gps_points_movebank_ECMWF_calc.device_info_serial = guillemots_gps_points_movebank_ecmwf.device_info_serial)
                          WHERE (((guillemots_gps_points_igu.date_time)>=[guillemots_track_session].[start_date] And (guillemots_gps_points_igu.date_time)<=[guillemots_track_session].[end_date]) AND ((guillemots_track_session.device_info_serial)=[guillemots_gps_points_igu].[device_info_serial]))
                          ORDER BY guillemots_gps_points_igu.device_info_serial, guillemots_gps_points_igu.date_time;
                          ",
                          as.is = TRUE)


# Before merging data ----
# 2009 IGU data - calculate track course
# Subset out 2009 IGU data
date_temp <- as.POSIXct(gpspoints.igu$date_time, tz = "UTC")

igu_2009 <- date_temp < as.POSIXct("2010-01-01 00:00:00", tz = "UTC")
summary(igu_2009)

library(fossil)
n <- sum(igu_2009)
dir.igu <- earth.bear(gpspoints.igu$longitude[igu_2009][-1],
            gpspoints.igu$latitude[igu_2009][-1],
             gpspoints.igu$longitude[igu_2009][-n],
             gpspoints.igu$latitude[igu_2009][-n])
dir.igu.2009 <- c(0, dir.igu)

dir.new.igu <- gpspoints.igu$course
dir.new.igu[igu_2009] <- dir.igu.2009

# UVA data - calculate course to 0-360 range
correct.bear <- function(x){
  if(x <0){
    return(x+360)
  } else return(x)
}

dir.new.uva <- sapply(gpspoints.uva$direction, correct.bear)
# 
# hist(dir.new.uva)
# hist(dir.new.igu)
# range(dir.new.igu)
# range(dir.new.uva)
# hist(gpspoints.igu$course)
# 
# x <- gpspoints.igu[(gpspoints.igu$course == 0) & !is.na(gpspoints.igu$course),]


# Merge data from IGU and UVA ----
# Make sure data are in common format, with same important columns.
gpspoints.uva$direction <- dir.new.uva
gpspoints.igu$course <- dir.new.igu

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





# Flight vector components ------------------------

#' Calculating heading vector (we already have the wind vector
#' and the track vector).
#' In principle this is simple vector addition.
#' 

# Calculate heading vectors from speed and direction


# test <- 

# gpspoints$course[1:10]
# test[1:10]

veast <- gpspoints$speed_ms*(sin(gpspoints$course*((pi)/(180))))
hist(veast)

vnorth <- gpspoints$speed_ms*(cos(gpspoints$course*((pi)/(180))))
hist(vnorth)

# Calculating u and v component of heading vector
# Subtract wind vector from ground vector, leaving
# heading vector
# names(points)
# names(points.weather)
head_u_ecmwf <- veast  - gpspoints$wind_u_10m_flt_ht
head_v_ecmwf <- vnorth - gpspoints$wind_v_10m_flt_ht
hist(head_u_ecmwf)
hist(head_v_ecmwf)


source("wind_dir_speed.R")


head.info <- t(mapply(wind.dir.speed, head_u_ecmwf,
                      head_v_ecmwf))
# hist(head.info[,1], breaks = 1000, ylim = c(0,200), xlim = c(0,25))
# 
# mean(head.info[head.info[,1] > 11,1], na.rm = TRUE)

# names(points.weather)
# Make dataframe
head.info <- as.data.frame(cbind(head.info, head_u_ecmwf, head_v_ecmwf))

# Give names to columns
names(head.info) <- c("head_speed_ecmwf", "head_dir_ecmwf", "head_u_ecmwf", "head_v_ecmwf")


# Ground speed + track heading
ground_heading <- gpspoints$course
ground_speed   <- gpspoints$speed_ms



# Flight vectors - relative to track ----------

bear_cor <- function(x, track){
  out <- x - track
  out <- out %% 360
  #   if(out > 180) return(360 - out) else return(out)
}

head_dir <-  mapply(bear_cor,
                    x = gps.data.par$head_dir_ecmwf,
                    track = gps.data.par$ground_heading)

str(gps.data.par)
wind_dir <-   mapply(bear_cor,
                     x = gps.data.par$wind_dir_ecmwf,
                     track = gps.data.par$ground_heading)



par <- cbind(wind_dir, gps.data.par$wind_speed_flt_ht_ecmwf,
             head_dir, gps.data.par$head_speed,
             gps.data.par$ground_speed)

par <- as.data.frame(par)
names(par) <- c("wind_dir","wind_speed","head_dir","head_speed","track_speed")

head(par)


# Function to get alpha or beta, if angle is reflex then subtract it from 360
ang.cor <- function(x){
  if(is.na(x)) return(x) else{
    if(x > 180) return(360 - x)
    else return(x)}
}

# alpha <- NULL
alpha <- sapply(X = par$head_dir, FUN = ang.cor)

beta <- sapply(X = par$wind_dir, FUN = ang.cor)

par <- cbind(par, alpha, beta)

# side and head wind components
wind.comp <- function(beta, wind_speed, wind_dir){
  if(is.na(beta) | is.na(wind_speed) | is.na(wind_dir)){
    return(c(NA,NA))} else {
      # Package needed to convert degrees to radians
      require(CircStats)
      # If beta angle is more than 90 do these calculations
      if(beta > 90){
        beta <- 180 - beta
        beta.rad <- rad(beta)
        
        wind_head_tail <- (cos(beta.rad))*wind_speed
        # As beta >90 wind must be tails wind, make negative
        wind_head_tail <- wind_head_tail * -1
        
        wind_side <- (sin(beta.rad))*wind_speed
        # If wind comes from left make negative
        if(wind_dir < 180)  wind_side <- wind_side * -1
      } else {
        beta.rad <- rad(beta)
        wind_head_tail <- (cos(beta.rad))*wind_speed
        wind_side <- (sin(beta.rad))*wind_speed
        if(wind_dir < 180)  wind_side <- wind_side * -1
      }
      # For testing, check that calculated side wind and head wind components would add up to original wind vector (i.e. wind speed)
      #   test_var <- sqrt((wind_side*wind_side) + (wind_head_tail*wind_head_tail))
      #   return(c(wind_side, wind_head_tail, test_var))
      return(c(wind_side, wind_head_tail))
    }
}

wind.comp_calc <- mapply(wind.comp,
                         beta = par$beta,
                         wind_speed = par$wind_speed,
                         wind_dir = par$wind_dir) 

wind.comp_calc_10 <- mapply(wind.comp,
                            beta = par$beta,
                            wind_speed = gps.data.par$wind_speed_10m_ecmwf,
                            wind_dir = par$wind_dir) 



names(gps.data)
names(gps.data.par)
alpha.new <- gps.data.par$ground_heading - gps.data.par$head_dir


alpha.calc <- function(head, track){
  if(is.na(track) | is.na(head)){
    theta = NA
  }else{
    theta <- track - head
    if(abs(theta) > 180){
      theta <- 360 - abs(theta)
    }
    theta <- abs(theta)
    if(head > track){
      theta <- theta*-1
    }
  }
  return(theta)  
}

alpha.new <- mapply(alpha.calc, gps.data.par$head_dir, 
                    gps.data.par$ground_heading)




