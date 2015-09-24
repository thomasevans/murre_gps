


# Read in DB data -----
# DB package
library("RODBC")

# Connect to DB
gps.db <- odbcConnectAccess2007('D:/Dropbox/tracking_db/murre_db/murre_db.accdb')


# Get uva data
gpspoints.uva <- sqlQuery(gps.db,
                          query = "SELECT uvabits_gps.device_info_serial, uvabits_gps.date_time, uvabits_gps.latitude, uvabits_gps.longitude, uvabits_gps.altitude, uvabits_gps.speed_2d, guillemots_track_session.ring_number, guillemots_gps_points_uva_class.coldist, guillemots_gps_points_uva_class.type, guillemots_gps_points_trip_id.trip_id, guillemots_gps_points_trip_id.deploy_id, guillemots_gps_points_flight_id.flight_id
FROM guillemots_track_session, guillemots_gps_points_flight_id INNER JOIN (guillemots_gps_points_trip_id INNER JOIN (guillemots_gps_points_uva_class INNER JOIN uvabits_gps ON (guillemots_gps_points_uva_class.device_info_serial = uvabits_gps.device_info_serial) AND (guillemots_gps_points_uva_class.date_time = uvabits_gps.date_time)) ON (guillemots_gps_points_trip_id.date_time = uvabits_gps.date_time) AND (guillemots_gps_points_trip_id.device_info_serial = uvabits_gps.device_info_serial)) ON (guillemots_gps_points_flight_id.date_time = guillemots_gps_points_trip_id.date_time) AND (guillemots_gps_points_flight_id.device_info_serial = guillemots_gps_points_trip_id.device_info_serial)
                          WHERE (((uvabits_gps.date_time)>=[guillemots_track_session].[start_date] And (uvabits_gps.date_time)<=[guillemots_track_session].[end_date]) AND ((uvabits_gps.latitude)<>0) AND ((uvabits_gps.longitude)<>0) AND ((guillemots_track_session.device_info_serial)=[uvabits_gps].[device_info_serial]))
                          ORDER BY uvabits_gps.device_info_serial, uvabits_gps.date_time;
                          ",
                          as.is = TRUE)


# Get igu data
gpspoints.igu <- sqlQuery(gps.db,
                          query = "SELECT guillemots_gps_points_igu.device_info_serial, guillemots_gps_points_igu.date_time, guillemots_gps_points_igu.latitude, guillemots_gps_points_igu.longitude, guillemots_gps_points_igu.elev, guillemots_gps_points_igu.speed_ms, guillemots_track_session.ring_number, guillemots_gps_points_igu_class.coldist, guillemots_gps_points_igu_class.type, guillemots_gps_points_trip_id.trip_id, guillemots_gps_points_trip_id.deploy_id, guillemots_gps_points_flight_id.flight_id
FROM guillemots_track_session, guillemots_gps_points_flight_id INNER JOIN (guillemots_gps_points_trip_id INNER JOIN (guillemots_gps_points_igu_class INNER JOIN guillemots_gps_points_igu ON (guillemots_gps_points_igu_class.date_time = guillemots_gps_points_igu.date_time) AND (guillemots_gps_points_igu_class.device_info_serial = guillemots_gps_points_igu.device_info_serial)) ON (guillemots_gps_points_trip_id.date_time = guillemots_gps_points_igu_class.date_time) AND (guillemots_gps_points_trip_id.device_info_serial = guillemots_gps_points_igu_class.device_info_serial)) ON (guillemots_gps_points_igu.date_time = guillemots_gps_points_flight_id.date_time) AND (guillemots_gps_points_flight_id.device_info_serial = guillemots_gps_points_igu.device_info_serial)
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
flight_end <- NULL
flight_id <- NULL
flight_device_info_serial <- NULL
flight_ring_number <- NULL
flight_col_dist_dif <- flight_col_dist_start <- flight_col_dist_end <- NULL
flight_dep_id <- NULL
n_points <- trip_id <- NULL
flight_duration <- dist_straight <- p2p2_dist <- NULL
trip_id <- NULL
speed_mean <- speed_median <- speed_max <- NULL
elev_mean <- elev_median <- elev_max <- elev_min <- NULL
device_type <- NULL


source("deg.dist.R")
source("m_deg_dist.R")

# i <- 12

for(i in 1:length(flight_ids)){
#   for (i in 1:20){
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
                               points.sub$latitude[n],
                               km = FALSE) 
  
  # point-2-point distance
  if(n >= 2){
    p2p2_dist[i] <- sum(m.deg.dist(points.sub$longitude, points.sub$latitude))
  } else  p2p2_dist[i] <- 0
  
  # For speeds only include that actual flying points, so only include
  # fixes, where the speed is >5 ms-1
  f <- points.sub$speed_ms > 5
  
  if(sum(f) > 1){
    # Speed mean
    speed_mean[i] <- mean(points.sub$speed_ms[f], na.rm = TRUE)
    
    # Speed max
    speed_max[i] <- max(points.sub$speed_ms[f], na.rm = TRUE)
    
    # Speed median
    speed_median[i] <- median(points.sub$speed_ms[f], na.rm = TRUE)
    
    # As for speeds - only include the actual flight points here
    # Altitude mean
    elev_mean[i] <- mean(points.sub$elev[f], na.rm = TRUE)
    
    # Altitude median
    elev_median[i] <- median(points.sub$elev[f], na.rm = TRUE)
    
    # Altitude max
    elev_max[i] <- max(points.sub$elev[f], na.rm = TRUE)
    
    # Altitude min
    elev_min[i] <- min(points.sub$elev[f], na.rm = TRUE)
        
  } else {
    # Speed mean
    speed_mean[i] <- NA
    
    # Speed max
    speed_max[i] <- NA
    
    # Speed median
    speed_median[i] <- NA
    
    # As for speeds - only include the actual flight points here
    # Altitude mean
    elev_mean[i] <- NA
    
    # Altitude median
    elev_median[i] <- NA
    
    # Altitude max
    elev_max[i] <- NA
    
    # Altitude min
    elev_min[i] <- NA
  }
  
  a <- substring(as.character(flight_device_info_serial[i]), 1, 1)
  b <- a == "g"
  if(b){ device_type[i] <- "igu" } else device_type[i] <- "uva"
  
  
}


# Compile flight table (before adding trip details) -----

# Correct data format
flight_start <- as.POSIXct(flight_start, origin = "1970-01-01",
                           tz = "UTC")
flight_end <- as.POSIXct(flight_end, origin = "1970-01-01",
                         tz = "UTC")
flight_device_info_serial <- as.factor(flight_device_info_serial)
flight_ring_number <- as.factor(flight_ring_number)
flight_dep_id <- as.factor(flight_dep_id)
flight_id <- as.factor(flight_id)
device_type <- as.factor(device_type)

flights.df <- data.frame(flight_id, flight_ring_number, flight_dep_id,
                         flight_device_info_serial, flight_start, flight_end,
                         n_points, flight_col_dist_start,
                         flight_col_dist_end, flight_col_dist_dif,
                          trip_id, flight_duration,
                         dist_straight, p2p2_dist, speed_mean, speed_max, speed_median,
                         elev_mean, elev_median, elev_max, elev_min, device_type)

str(flights.df)

names(flights.df) <- c("flight_id",  "ring_number",  "deploy_id", 
                         "device_info_serial",  "start_time",  "end_time", 
                         "n_points",  "col_dist_start", 
                         "col_dist_end",  "col_dist_dif", 
                          "trip_id",  "duration", 
                         "dist_straight",  "p2p2_dist",  "speed_mean",
                       "speed_max",  "speed_median", 
                         "alt_mean",  "alt_median",  "alt_max",  "alt_min",
                       "device_type")

# See how the data looks
hist(flights.df$dist_straight)
hist(flights.df$elev_median[flights.df$elev_median >-50 & flights.df$elev_median <100])


str(flights.df)


# Add flight type, and number on trip etc ------
# For each trip, label flights by flight number, and type
# I.e. first, final, other
trip_ids <- unique(flights.df$trip_id)
trip_ids <- trip_ids[trip_ids != 0]

nt <- length(trip_ids)

nf <- length(flights.df$flight_id)

flight_type <- rep("unclassified", nf)
flight_trip_n <- rep(NA, nf)

# i <- 3

# For each trip
for(i in 1:nt){
  
  # Filter flights to those that include this trip
  id <- trip_ids[i]
  flights.sub <- subset(flights.df, flights.df$trip_id == id)
  
  # Filter of whole flights table
  f <- flights.df$trip_id == id
  
  
  # Count number
  n <- length(flights.sub$flight_id)
  
  # Label all these flights to type 'other'
  flight_type[f] <- "other"
  
  
  # Then if >= 2
  if(n >=2){
    # Label #1 as 'out'
    flight_type[f][1] <- "first"
    # Label #n as 'in'
    flight_type[f][n] <- "final"    
  }

  
  # Label all the flights by actual number
  # For i in length of n_flights
#   x
  for(x in 1:n){
    flight_trip_n[f][x] <- x
  }
  
  
}
# End of for each trip


# Add these details to the flight table
flights.df <- cbind(flights.df, flight_type ,flight_trip_n)
  
  names(flights.df)[23:24] <- c("type", "trip_flight_n") 
  
str(flights.df)
  #
  
  
  




# Output to DB ----
# will be neccessary to edit table in Access after to define data-types and primary keys and provide descriptions for each variable.
sqlSave(gps.db, flights.df,
        tablename = "guillemots_gps_flights",
        append = FALSE, rownames = FALSE, colnames = FALSE,
        verbose = FALSE, safer = TRUE, addPK = FALSE, fast = TRUE,
        test = FALSE, nastring = NULL,
        varTypes =  c(start_time = "datetime",
                      end_time = "datetime")
)





