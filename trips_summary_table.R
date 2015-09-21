


# Read in DB data -----
# DB package
library("RODBC")

# Connect to DB
gps.db <- odbcConnectAccess2007('D:/Dropbox/tracking_db/murre_db/murre_db.accdb')


# Get uva data
gpspoints.uva <- sqlQuery(gps.db,
                          query = "SELECT uvabits_gps.device_info_serial, uvabits_gps.date_time, uvabits_gps.latitude, uvabits_gps.longitude, guillemots_track_session.ring_number, guillemots_gps_points_uva_class.coldist, guillemots_gps_points_uva_class.type, guillemots_gps_points_trip_id.trip_id, guillemots_gps_points_trip_id.deploy_id
FROM guillemots_track_session, guillemots_gps_points_trip_id INNER JOIN (guillemots_gps_points_uva_class INNER JOIN uvabits_gps ON (guillemots_gps_points_uva_class.device_info_serial = uvabits_gps.device_info_serial) AND (guillemots_gps_points_uva_class.date_time = uvabits_gps.date_time)) ON (guillemots_gps_points_trip_id.date_time = uvabits_gps.date_time) AND (guillemots_gps_points_trip_id.device_info_serial = uvabits_gps.device_info_serial)
                          WHERE (((uvabits_gps.date_time)>=[guillemots_track_session].[start_date] And (uvabits_gps.date_time)<=[guillemots_track_session].[end_date]) AND ((uvabits_gps.latitude)<>0) AND ((uvabits_gps.longitude)<>0) AND ((guillemots_track_session.device_info_serial)=[uvabits_gps].[device_info_serial]))
                          ORDER BY uvabits_gps.device_info_serial, uvabits_gps.date_time;
                          ",
                          as.is = TRUE)


# Get igu data
gpspoints.igu <- sqlQuery(gps.db,
                          query = "SELECT guillemots_gps_points_igu.device_info_serial, guillemots_gps_points_igu.date_time, guillemots_gps_points_igu.latitude, guillemots_gps_points_igu.longitude, guillemots_track_session.ring_number, guillemots_gps_points_igu_class.coldist, guillemots_gps_points_igu_class.type, guillemots_gps_points_trip_id.trip_id, guillemots_gps_points_trip_id.deploy_id
FROM guillemots_track_session, guillemots_gps_points_trip_id INNER JOIN (guillemots_gps_points_igu_class INNER JOIN guillemots_gps_points_igu ON (guillemots_gps_points_igu_class.date_time = guillemots_gps_points_igu.date_time) AND (guillemots_gps_points_igu_class.device_info_serial = guillemots_gps_points_igu.device_info_serial)) ON (guillemots_gps_points_trip_id.date_time = guillemots_gps_points_igu_class.date_time) AND (guillemots_gps_points_trip_id.device_info_serial = guillemots_gps_points_igu_class.device_info_serial)
                          WHERE (((guillemots_gps_points_igu.date_time)>=[guillemots_track_session].[start_date] And (guillemots_gps_points_igu.date_time)<=[guillemots_track_session].[end_date]) AND ((guillemots_gps_points_igu.latitude)<>0) AND ((guillemots_gps_points_igu.longitude)<>0) AND ((guillemots_track_session.device_info_serial)=[guillemots_gps_points_igu].[device_info_serial]))
                          ORDER BY guillemots_gps_points_igu.device_info_serial, guillemots_gps_points_igu.date_time;
                          ",
                          as.is = TRUE)


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






# Assemble table of foraging trips -----
# - trip_id
# - start_time
# - end_time
# - device_id
# - ring_number

trip_ids <- unique(gpspoints$trip_id)[-1]
head(trip_ids)


trip_start <- NULL
trip_end <- NULL
trip_id <- NULL
trip_device_info_serial <- NULL
trip_ring_number <- NULL
trip_col_dist <- NULL
trip_dep_id <- NULL


# i <- 12

for(i in 1:length(trip_ids)){
  id <- trip_ids[i]
  points.sub <- subset(gpspoints, gpspoints$trip_id == id)
  
  trip_start[i] <- min(points.sub$date_time)
  trip_end[i]   <- max(points.sub$date_time)
  trip_id[i]    <- id
  trip_col_dist[i]  <- max(points.sub$coldist, na.rm = TRUE)
  trip_device_info_serial[i] <- as.character(
    points.sub$device_info_serial[1])
  trip_ring_number[i] <- as.character(points.sub$ring_number[1])
  trip_dep_id[i] <- as.character(points.sub$deploy_id[1])
}


# trip_start[i]

# Correct data format
trip_start <- as.POSIXct(trip_start, origin = "1970-01-01",
                         tz = "UTC")
trip_end <- as.POSIXct(trip_end, origin = "1970-01-01",
                       tz = "UTC")
trip_device_info_serial <- as.factor(trip_device_info_serial)
trip_ring_number <- as.factor(trip_ring_number)
trip_dep_id <- as.factor(trip_dep_id)

trips.df <- data.frame(trip_id,
                       trip_device_info_serial,
                       trip_start,
                       trip_end,
                       trip_ring_number,
                       trip_dep_id,
                       trip_col_dist)

str(trips.df)

names(trips.df) <- c("trip_id",
                     "device_info_serial",
                     "start_time",
                     "end_time",
                     "ring_number",
                     "deploy_id",
                     "col_dist_max")




# Output to DB ----
#will be neccessary to edit table in Access after to define data-types and primary keys and provide descriptions for each variable.
sqlSave(gps.db, trips.df,
        tablename = "guillemots_gps_trips",
        append = FALSE, rownames = FALSE, colnames = FALSE,
        verbose = FALSE, safer = TRUE, addPK = FALSE, fast = TRUE,
        test = FALSE, nastring = NULL,
        varTypes =  c(start_time = "datetime",
                      end_time = "datetime")
)





