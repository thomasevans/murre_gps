# Primarily developed by Tom Evans at Lund University: tom.evans@biol.lu.se
# You are welcome to use parts of this code, but please give credit when using it extensively.

# Classification of guillemot GPS data into foraging trips
# Will base this largely on the script used to do this for the LBBG data,
# adapting slightly the various thresholds etc.
# Purpose of this script is simply to identify foraging trips,
# Then get start-time, end-time, and number each trip with unique ID
# Summary statistics for each foraging trip will be extracted in a
# second sepperate script.



# Read in DB data -----
# DB package
library("RODBC")

# Connect to DB
gps.db <- odbcConnectAccess2007('D:/Dropbox/tracking_db/murre_db/murre_db.accdb')


# Get uva data
gpspoints.uva <- sqlQuery(gps.db,
                        query = "SELECT uvabits_gps.device_info_serial, uvabits_gps.date_time, uvabits_gps.latitude, uvabits_gps.longitude, guillemots_track_session.ring_number, guillemots_gps_points_uva_class.coldist, guillemots_gps_points_uva_class.type
FROM guillemots_track_session, guillemots_gps_points_uva_class INNER JOIN uvabits_gps ON (guillemots_gps_points_uva_class.date_time = uvabits_gps.date_time) AND (guillemots_gps_points_uva_class.device_info_serial = uvabits_gps.device_info_serial)
                        WHERE (((uvabits_gps.date_time)>=[guillemots_track_session].[start_date] And (uvabits_gps.date_time)<=[guillemots_track_session].[end_date]) AND ((uvabits_gps.latitude)<>0) AND ((uvabits_gps.longitude)<>0) AND ((guillemots_track_session.device_info_serial)=[uvabits_gps].[device_info_serial]))
                        ORDER BY uvabits_gps.device_info_serial, uvabits_gps.date_time;
                        ",
                        as.is = TRUE)


# Get igu data
gpspoints.igu <- sqlQuery(gps.db,
                          query = "SELECT guillemots_gps_points_igu.device_info_serial, guillemots_gps_points_igu.date_time, guillemots_gps_points_igu.latitude, guillemots_gps_points_igu.longitude, guillemots_track_session.ring_number, guillemots_gps_points_igu_class.coldist, guillemots_gps_points_igu_class.type
FROM guillemots_track_session, guillemots_gps_points_igu_class INNER JOIN guillemots_gps_points_igu ON (guillemots_gps_points_igu.date_time = guillemots_gps_points_igu_class.date_time) AND (guillemots_gps_points_igu_class.device_info_serial = guillemots_gps_points_igu.device_info_serial)
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



# Unique track occasion
# combine year and ring_number
years <- format(gpspoints$date_time, "%Y")
dep_id <- paste(gpspoints$ring_number, years, sep = "_")


# Sort data by deployment id then date_time
ord <- order(dep_id, gpspoints$date_time)
x <- cbind(gpspoints, dep_id, years)
# Use this for res of analysis
points_all <- x[ord,]




# Classify into foraging trips -------
# (see previous
# analysis for the LBBG in 'export_files.R' lines 75 onwards)

# Label points by on foraging trip or not
on_trip <- ifelse(points_all$coldist < 200, 0,1)
summary(as.factor(on_trip))
points_all$on_trip <- on_trip


# Remove 'bad_location' points ----
# Index for 'bad_locations'
# Index for all location first
id <- c(1:length(points_all$type))
x <- points_all$type == "bad_location"
id.bad_location <- id[x]

# Excl bad locations
points_all <- points_all[-id.bad_location,]


# Label trip ID ------

# We want to label the positions for each trip with a unique trip id
# first we make some vectors of next, previous point etc, to find start
# and end points of trips
trip1 <- points_all$on_trip +1

#make vector of next point value
trip2 <- (2* c(points_all$on_trip[2:length(points_all$on_trip)],0))+1

#make vector of prev point value
trip3 <- (3* c(0,points_all$on_trip[1:(length(points_all$on_trip)-1)]))+1


#label by type of point: 0 - trip, 1 - start, 2 - end, 3 - nest
loc_type <- trip1*trip2*trip3   #product of above three vectors
loc_calc     <- loc_type        #keep a copy of above calculation

# summary(as.factor(loc_type))
# head(loc_calc)
# summary(as.factor(loc_calc))


#label by type of point: 0 - nest, 1 - start, 2 - end, 3 - trip

#Reduce to the four possibilties
loc_type[(loc_type == 1)  ]  <- 0
loc_type[loc_type == 3 | (loc_type == 12)] <- 1
loc_type[(loc_type == 24) | (loc_type == 6) 
         | (loc_type == 8) | (loc_type == 2)]<- 3
loc_type[loc_type == 4] <- 2
summary(as.factor(loc_type))

# summary(!is.na(points_all$on_trip))
# Fix for deployment transitions
# Adding '4' for first point of new deployment
t <- 0
for( i in 2: length(loc_type)){
  if(points_all$dep_id[i] !=
     points_all$dep_id[i-1]){
    loc_type[i] <- 4
    t <- t + 1
  }   
}
# t



trip_id <- rep(0,length(loc_type))


# Loop through all points
#x will keep note of trip number, we start at zero.
x <- 0
n <- length(loc_type)
ind <- c(1:n)
new_deployment <- FALSE


for(i in 1:n){
  if(loc_type[i] == 4){new_deployment <- TRUE
  trip_id[i] <- 0
  }
  if(loc_type[i] != 0 & loc_type[i] != 4){
    #if start of a trip, increment x by one
    if(loc_type[i] == 1) {x <- x + 1
    new_deployment <- FALSE}
    if(new_deployment == TRUE & (loc_type[i]  != 0
                             & loc_type[i]    != 1
                             & loc_type[i]    != 2)) {
      x <- x + 1
      new_deployment <- FALSE}
    
    # allocated value of x for trip_id for position 'i'.
    trip_id[i] <- x    
  }
}






# Output details to DB
# Trip classification

# GPS details table -----
# - device_info_serial
# - date_time
# - trip_id
# - unique device_bird id??
gps_info <- data.frame(points_all$device_info_serial,
                       points_all$date_time,
                       points_all$ring_number,
                       trip_id,
                       points_all$dep_id
                       )
str(gps_info)
names(gps_info)  <-  c("device_info_serial",
                       "date_time",
                       "ring_number",
                       "trip_id",
                       "deploy_id")


# Output to DB ----
#will be neccessary to edit table in Access after to define data-types and primary keys and provide descriptions for each variable.
sqlSave(gps.db, gps_info,
        tablename = "guillemots_gps_points_trip_id",
        append = FALSE, rownames = FALSE, colnames = FALSE,
        verbose = FALSE, safer = TRUE, addPK = FALSE, fast = TRUE,
        test = FALSE, nastring = NULL,
        varTypes =  c(date_time = "datetime")
)
#
