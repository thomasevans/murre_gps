# Primarily developed by Tom Evans at Lund University: tom.evans@biol.lu.se
# You are welcome to use parts of this code, but please give credit when using it extensively.

# Classification of guillemot GPS data into foraging flights
# Based on script used to classify foraging trips
# Purpose of this script is simply to identify sepperate flights,
# Then start-time, end-time, and number each trip with unique ID
# Summary statistics for each foraging trip will be extracted in a
# second sepperate script.



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




# Flight points -------
on_flight <- gpspoints$type == "flight"
summary(as.factor(on_flight))
gpspoints$on_flight <- on_flight



# Label trip ID ------

# We want to label the positions for each flight with a unique flight id
# first we make some vectors of next, previous point etc, to find start
# and end points of flights
flight1 <- gpspoints$on_flight +1

#make vector of next point value
flight2 <- (2* c(gpspoints$on_flight[2:length(gpspoints$on_flight)],0))+1

#make vector of prev point value
flight3 <- (3* c(0,gpspoints$on_flight[1:(length(gpspoints$on_flight)-1)]))+1

# summary(as.factor(flight3))

#label by type of point: 0 - flight, 1 - start, 2 - end, 3 - nest
flight_type <- flight1*flight2*flight3   #product of above three vectors
flight_calc     <- flight_type        #keep a copy of above calculation

summary(as.factor(flight_type))
head(flight_calc)
summary(as.factor(flight_calc))


#label by type of point: 0 - non-flight, 1 - start, 2 - end, 3 - in flight

#Reduce to the four possibilties
flight_type[(flight_type == 1)  ]  <- 0
flight_type[flight_type == 3 | (flight_type == 12)] <- 1
flight_type[(flight_type == 24) | (flight_type == 6) 
         | (flight_type == 8) | (flight_type == 2)]<- 3
flight_type[flight_type == 4] <- 2
summary(as.factor(flight_type))

# summary(!is.na(points_all$on_trip))
# Fix for deployment transitions
# Adding '4' for first point of new deployment
t <- 0
for( i in 2: length(flight_type)){
  if(gpspoints$deploy_id[i] !=
     gpspoints$deploy_id[i-1]){
    flight_type[i] <- 4
    t <- t + 1
  }   
}
# t



flight_id <- rep(0,length(flight_type))


# Loop through all points
# x will keep note of flight number, we start at zero.
x <- 0
n <- length(flight_type)
ind <- c(1:n)
new_deployment <- TRUE


for(i in 1:n){
  if(flight_type[i] == 4){new_deployment <- TRUE
   flight_id[i] <- 0
  x <- x + 1
  }
  if(flight_type[i] != 0 & flight_type[i] != 4){
    #if start of a trip, increment x by one
    if(flight_type[i] == 1) {x <- x + 1
    new_deployment <- FALSE}
    if(new_deployment == TRUE & (flight_type[i]  != 0
                                 & flight_type[i]    != 1
                                 & flight_type[i]    != 2)) {
      x <- x + 1
      new_deployment <- FALSE}
    
    # allocated value of x for trip_id for position 'i'.
    flight_id[i] <- x    
  }
}

# test <- points_all[trip_id == 160,]
# test2 <- cbind(points_all,loc_type,trip_id)
# (flight_id)


# Output details to DB
# Trip classification

# GPS details table -----
# - device_info_serial
# - date_time
# - trip_id
# - unique device_bird id??
gps_info <- data.frame(gpspoints$device_info_serial,
                       gpspoints$date_time,
                       gpspoints$ring_number,
                       flight_id,
                       gpspoints$deploy_id
                      )
str(gps_info)
names(gps_info)  <-  c("device_info_serial",
                       "date_time",
                       "ring_number",
                       "flight_id",
                       "deploy_id")


# Output to DB ----
#will be neccessary to edit table in Access after to define data-types and primary keys and provide descriptions for each variable.
sqlSave(gps.db, gps_info,
        tablename = "guillemots_gps_points_flight_id",
        append = FALSE, rownames = FALSE, colnames = FALSE,
        verbose = FALSE, safer = TRUE, addPK = FALSE, fast = TRUE,
        test = FALSE, nastring = NULL,
        varTypes =  c(date_time = "datetime")
)
#
