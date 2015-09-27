

# Get derived weather variables for each GPS location
# Based on script 'move_bank_data_compilation_wind_calc.R' in LBBG


# Datbase functions -----
# Get the flight data from the db.
library(RODBC)

# Establish a connection to the database
gps.db <- odbcConnectAccess2007('D:/Dropbox/tracking_db/murre_db/murre_db.accdb')



# Get uva data
gpspoints.uva <- sqlQuery(gps.db,
                          query = "SELECT uvabits_gps.device_info_serial, uvabits_gps.date_time, uvabits_gps.altitude, uvabits_gps.speed_2d, guillemots_track_session.ring_number, guillemots_gps_points_trip_id.trip_id, guillemots_gps_points_trip_id.deploy_id, guillemots_gps_points_flight_id.flight_id, guillemots_gps_points_movebank_ecmwf.ecmwf_wind_10m_v, guillemots_gps_points_movebank_ecmwf.ecmwf_wind_10m_u, guillemots_gps_points_movebank_ecmwf.ecmwf_surf_roughness, guillemots_gps_points_movebank_ecmwf.ecmwf_charnock, guillemots_gps_points_movebank_ecmwf.ecmwf_boundary_lay_ht
FROM guillemots_track_session, guillemots_gps_points_movebank_ecmwf INNER JOIN (guillemots_gps_points_flight_id INNER JOIN (guillemots_gps_points_trip_id INNER JOIN (guillemots_gps_points_uva_class INNER JOIN uvabits_gps ON (guillemots_gps_points_uva_class.date_time = uvabits_gps.date_time) AND (guillemots_gps_points_uva_class.device_info_serial = uvabits_gps.device_info_serial)) ON (guillemots_gps_points_trip_id.device_info_serial = uvabits_gps.device_info_serial) AND (guillemots_gps_points_trip_id.date_time = uvabits_gps.date_time)) ON (guillemots_gps_points_flight_id.device_info_serial = guillemots_gps_points_trip_id.device_info_serial) AND (guillemots_gps_points_flight_id.date_time = guillemots_gps_points_trip_id.date_time)) ON (guillemots_gps_points_flight_id.date_time = guillemots_gps_points_movebank_ecmwf.date_time) AND (guillemots_gps_points_movebank_ecmwf.device_info_serial = guillemots_gps_points_flight_id.device_info_serial)
                          WHERE (((uvabits_gps.date_time)>=[guillemots_track_session].[start_date] And (uvabits_gps.date_time)<=[guillemots_track_session].[end_date]) AND ((guillemots_track_session.device_info_serial)=[uvabits_gps].[device_info_serial]))
                          ORDER BY uvabits_gps.device_info_serial, uvabits_gps.date_time;
                          ",
                          as.is = TRUE)


# Get igu data
gpspoints.igu <- sqlQuery(gps.db,
                          query = "SELECT guillemots_gps_points_igu.device_info_serial, guillemots_gps_points_igu.date_time, guillemots_gps_points_igu.elev, guillemots_gps_points_igu.speed_ms, guillemots_track_session.ring_number, guillemots_gps_points_trip_id.trip_id, guillemots_gps_points_trip_id.deploy_id, guillemots_gps_points_flight_id.flight_id, guillemots_gps_points_movebank_ecmwf.ecmwf_wind_10m_v, guillemots_gps_points_movebank_ecmwf.ecmwf_wind_10m_u, guillemots_gps_points_movebank_ecmwf.ecmwf_surf_roughness, guillemots_gps_points_movebank_ecmwf.ecmwf_charnock, guillemots_gps_points_movebank_ecmwf.ecmwf_boundary_lay_ht
FROM guillemots_track_session, (guillemots_gps_points_flight_id INNER JOIN (guillemots_gps_points_trip_id INNER JOIN (guillemots_gps_points_igu_class INNER JOIN guillemots_gps_points_igu ON (guillemots_gps_points_igu_class.date_time = guillemots_gps_points_igu.date_time) AND (guillemots_gps_points_igu_class.device_info_serial = guillemots_gps_points_igu.device_info_serial)) ON (guillemots_gps_points_trip_id.date_time = guillemots_gps_points_igu_class.date_time) AND (guillemots_gps_points_trip_id.device_info_serial = guillemots_gps_points_igu_class.device_info_serial)) ON (guillemots_gps_points_flight_id.date_time = guillemots_gps_points_igu.date_time) AND (guillemots_gps_points_flight_id.device_info_serial = guillemots_gps_points_igu.device_info_serial)) INNER JOIN guillemots_gps_points_movebank_ecmwf ON (guillemots_gps_points_flight_id.date_time = guillemots_gps_points_movebank_ecmwf.date_time) AND (guillemots_gps_points_flight_id.device_info_serial = guillemots_gps_points_movebank_ecmwf.device_info_serial)
                          WHERE (((guillemots_gps_points_igu.date_time)>=[guillemots_track_session].[start_date] And (guillemots_gps_points_igu.date_time)<=[guillemots_track_session].[end_date]) AND ((guillemots_track_session.device_info_serial)=[guillemots_gps_points_igu].[device_info_serial]))
                          ORDER BY guillemots_gps_points_igu.device_info_serial, guillemots_gps_points_igu.date_time;",
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





# First adjust altitude (if < 1) new altitude is 1 m. -----
if.neg <- function(x){
  if(is.na(x)) return(NA) else{
    if(x < 1) return(1)
    else return(x)
  }
}

alt_new <- sapply(gpspoints$elev,if.neg)
hist(alt_new)

summary(alt_new == 1)

# Then wind shear ----
wind.shear <- function(wind10, alt, roughness){
  a <- log(alt/roughness)
  b <- log(10/roughness)
  c <- a/b
  wind10*c  
}

# min(wind_data$alt)
wind.calculated.u <- wind.shear(gpspoints$ecmwf_wind_10m_u, alt_new,
                                gpspoints$ecmwf_surf_roughness)

wind.calculated.v <- wind.shear(gpspoints$ecmwf_wind_10m_v, alt_new,
                                gpspoints$ecmwf_surf_roughness)



# Direction and scalar speed ----
source("wind_dir_speed.R")

wind.10 <- t(mapply(wind.dir.speed,
                    gpspoints$ecmwf_wind_10m_u, gpspoints$ecmwf_wind_10m_v))
# hist(wind.10[,1])

wind.flight <- t(mapply(wind.dir.speed,
                        wind.calculated.u, wind.calculated.v))

# Assemble to a single table to output to database ----
out.table <- cbind(gpspoints$device_info_serial,
                   gpspoints$date_time,
                   gpspoints$deploy_id,
                   gpspoints$flight_id,
                   wind.calculated.u,
                   wind.calculated.v,
                   wind.flight[,1],
                   wind.flight[,2],
                   wind.10[,1])
str(out.table)

out.table <- as.data.frame(out.table)
str(out.table)

names(out.table) <- c("device_info_serial", "date_time",
                      "deploy_id", "flight_id",
                      "wind_u_10m_flt_ht", "wind_v_10m_flt_ht",
                      "wind_speed_flt_ht",
                      "wind_dir",
                      "wind_speed_10m")
out.table$date_time <- gpspoints$date_time

str(out.table)

# Output to new table in the database. ----
sqlSave(gps.db, out.table, tablename = "guillemots_gps_points_movebank_ECMWF_calc",
        append = FALSE, rownames = FALSE, colnames = FALSE,
        verbose = FALSE, safer = TRUE, addPK = FALSE, fast = TRUE,
        test = FALSE, nastring = NULL,
        varTypes =  c(date_time = "datetime",
                      wind_u_10m_flt_ht = "double",
                      wind_v_10m_flt_ht = "double",
                      wind_speed_flt_ht = "double",
                      wind_dir = "double",
                      wind_speed_10m = "double"))
# ?sqlSave

