


# Read in DB data -----
# DB package
library("RODBC")

# Connect to DB
gps.db <- odbcConnectAccess2007('D:/Dropbox/tracking_db/murre_db/murre_db.accdb')


# Get uva data
gpspoints.uva <- sqlQuery(gps.db,
                          query = "SELECT uvabits_gps.device_info_serial, uvabits_gps.date_time, uvabits_gps.latitude, uvabits_gps.longitude, guillemots_track_session.ring_number, guillemots_gps_points_uva_class.coldist, guillemots_gps_points_uva_class.type, guillemots_gps_points_trip_id.trip_id, guillemots_gps_points_trip_id.deploy_id, guillemots_gps_points_flight_id.flight_id
FROM guillemots_track_session, guillemots_gps_points_flight_id INNER JOIN (guillemots_gps_points_trip_id INNER JOIN (guillemots_gps_points_uva_class INNER JOIN uvabits_gps ON (guillemots_gps_points_uva_class.device_info_serial = uvabits_gps.device_info_serial) AND (guillemots_gps_points_uva_class.date_time = uvabits_gps.date_time)) ON (guillemots_gps_points_trip_id.date_time = uvabits_gps.date_time) AND (guillemots_gps_points_trip_id.device_info_serial = uvabits_gps.device_info_serial)) ON (guillemots_gps_points_flight_id.date_time = guillemots_gps_points_trip_id.date_time) AND (guillemots_gps_points_flight_id.device_info_serial = guillemots_gps_points_trip_id.device_info_serial)
                          WHERE (((uvabits_gps.date_time)>=[guillemots_track_session].[start_date] And (uvabits_gps.date_time)<=[guillemots_track_session].[end_date]) AND ((uvabits_gps.latitude)<>0) AND ((uvabits_gps.longitude)<>0) AND ((guillemots_track_session.device_info_serial)=[uvabits_gps].[device_info_serial]))
                          ORDER BY uvabits_gps.device_info_serial, uvabits_gps.date_time;
                          ",
                          as.is = TRUE)


# Get igu data
gpspoints.igu <- sqlQuery(gps.db,
                          query = "SELECT guillemots_gps_points_igu.device_info_serial, guillemots_gps_points_igu.date_time, guillemots_gps_points_igu.latitude, guillemots_gps_points_igu.longitude, guillemots_track_session.ring_number, guillemots_gps_points_igu_class.coldist, guillemots_gps_points_igu_class.type, guillemots_gps_points_trip_id.trip_id, guillemots_gps_points_trip_id.deploy_id, guillemots_gps_points_flight_id.flight_id
FROM guillemots_track_session, guillemots_gps_points_flight_id INNER JOIN (guillemots_gps_points_trip_id INNER JOIN (guillemots_gps_points_igu_class INNER JOIN guillemots_gps_points_igu ON (guillemots_gps_points_igu_class.date_time = guillemots_gps_points_igu.date_time) AND (guillemots_gps_points_igu_class.device_info_serial = guillemots_gps_points_igu.device_info_serial)) ON (guillemots_gps_points_trip_id.date_time = guillemots_gps_points_igu_class.date_time) AND (guillemots_gps_points_trip_id.device_info_serial = guillemots_gps_points_igu_class.device_info_serial)) ON (guillemots_gps_points_igu.date_time = guillemots_gps_points_flight_id.date_time) AND (guillemots_gps_points_flight_id.device_info_serial = guillemots_gps_points_igu.device_info_serial)
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








# Assemble table of flights -----
# - flight_id
# - start_time
# - end_time
# - device_id
# - ring_number

flight_ids <- unique(gpspoints$flight_id)[-1]
head(flight_ids)


flight_start <- NULL
flight_end <- NULL
flight_id <- NULL
flight_device_info_serial <- NULL
flight_ring_number <- NULL
flight_col_dist_dif <- flight_col_dist_start <- flight_col_dist_end <- NULL
flight_dep_id <- NULL
n_points <- trip_id <- NULL
flight_duration <- dist_straight <- p2p2_dist <- NULL


source("deg.dist.R")

i <- 12

for(i in 1:length(flight_ids)){
  id <- flight_ids[i]
  points.sub <- subset(gpspoints, gpspoints$flight_id == id)
  n <- n_points[i]  <- length(points.sub$device_info_serial)
  flight_start[i] <- min(points.sub$date_time)
  flight_end[i]   <- max(points.sub$date_time)
  flight_id[i]    <- id
  flight_col_dist_start[i]  <- points.sub$coldist[1]
  flight_col_dist_end[i]  <- points.sub$coldist[n_points[i]]
  flight_col_dist_dif[i] <-  flight_col_dist_end[i] - flight_col_dist_start[i]
  flight_device_info_serial[i] <- as.character(
    points.sub$device_info_serial[1])
  flight_ring_number[i] <- as.character(points.sub$ring_number[1])
  flight_dep_id[i] <- as.character(points.sub$deploy_id[1])
  trip_id[i] <- points.sub$trip_id[1]
  flight_duration[i] <- flight_end[i] - flight_start[i]
  
  # Summary calculations
  # Straight-line distance
  dist_straight[i] <- deg.dist(points.sub$longitude[1],
                               points.sub$latitude[1],
                               points.sub$longitude[n],
                               points.sub$latitude[n]) 
  
  # point-2-point distance
  
  p2p2_dist[i] <- sum(m.deg.dist(points.sub$longitude, points.sub$latitude))
  
  # For speeds only include that actual flying points, so only include
  # fixes, where the speed is >5 ms-1
  # Speed mean
  
  # Speed max
  
  # Speed median
  
  # As for speeds - only include the actual flight points here
  # Altitude mean
  
  # Altitude median
  
  # Altitude max
  
  # Altitude min
  
  
  
}


# For each trip, label flights by flight number, and type
# I.e. first, final, other




# Correct data format
flight_start <- as.POSIXct(flight_start, origin = "1970-01-01",
                         tz = "UTC")
flight_end <- as.POSIXct(flight_end, origin = "1970-01-01",
                       tz = "UTC")
flight_device_info_serial <- as.factor(flight_device_info_serial)
flight_ring_number <- as.factor(flight_ring_number)
flight_dep_id <- as.factor(flight_dep_id)

flights.df <- data.frame(flight_id,
                       flight_device_info_serial,
                       flight_start,
                       flight_end,
                       flight_ring_number,
                       flight_dep_id,
                       flight_col_dist)

str(flights.df)

names(flights.df) <- c("flight_id",
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





