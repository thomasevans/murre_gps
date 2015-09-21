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

# For 'bad_points' also label as 'NA', as colony distance cannot be
# relied on.
x <- points_all$type == "bad_location"
x[is.na(x)] <- FALSE
on_trip[x] <- NA

# Index for 'bad_locations'
# Index for all location first
id <- c(1:length(points_all$type))

id.bad_location <- id[x]

# summary((points_all$type))
# 866 - 263
# i <- 34

# no location and dive missing ...

# For each bad_location do:
for(i in 1:length(id.bad_location)){
  ind.loc <- id.bad_location[i]
  
  # Get index for last vallid location (!== "bad_location" | is.na is TRUE)
  # Work backward from current location
  
  xb <- "bad_location"
  n <- ind.loc
  while((xb == "no_location") | (xb == "bad_location") | is.na(xb)| is.na(on_trip[n])){
    n <- n -1
    xb <- points_all$type[n]
    if(points_all$device_info_serial[ind.loc] !=
       points_all$device_info_serial[n]) break
  }
  
  last_vallid <- on_trip[n]
  
  # Get index for next vallid location (as above)
  # Work forward from current location
  
  xb <- "bad_location"
  n <- ind.loc
  while((xb == "no_location") | (xb == "bad_location") | is.na(xb) | is.na(on_trip[n])){
    n <- n +1
    xb <- points_all$type[n]
    if(points_all$device_info_serial[ind.loc] !=
       points_all$device_info_serial[n]) break
  }
  
  next_vallid <- on_trip[n]
  
  
  if(is.na(next_vallid) | is.na(last_vallid)){
    on_trip[ind.loc] <- 0
  }else{
    # If on a transition, assume that it's not on a trip
    if(next_vallid == last_vallid) {on_trip[ind.loc] <-
      next_vallid} else {
        on_trip[ind.loc] <- 0
      }
  }
  
  
}




# summary(as.factor(on_trip))

on_trip_f <- on_trip[!is.na(on_trip)]
# summary(as.factor(on_trip_f))


# We want to label the positions for each trip with a unique trip id
# first we make some vectors of next, previous point etc, to find start
# and end points of trips
trip1 <- on_trip_f +1

#make vector of next point value
trip2 <- (2* c(on_trip_f[2:length(on_trip_f)],0))+1

#make vector of prev point value
trip3 <- (3* c(0,on_trip_f[1:(length(on_trip_f)-1)]))+1


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


# Fix for device transitions
# Adding '4' for first point of deployment
t <- 0
# []
#  i <- 1555
# length(points_all$ring_number[!is.na(on_trip_f)])
i <- i + 1
for( i in 2: length(loc_type)){
  if(points_all$ring_number[!is.na(on_trip)][i] !=
     points_all$ring_number[!is.na(on_trip)][i-1]){
    loc_type[i] <- 4
    t <- t + 1
  }   
}
# t

# points_all$ring_number[1550:1600]


trip_id <- rep(0,length(loc_type))


# Loop through all points (except for NAs)
#x will keep note of trip number, we start at zero.
x <- 0
n <- length(loc_type)
ind <- c(1:n)
# fours <- NULL
# t <- 1
new_device <- FALSE

# i <- ind[trip_id == 21]

for(i in 1:n){
  if(loc_type[i] == 4){new_device <- TRUE
  #   fours[t] <- i
  #   t <- t + 1}
  trip_id[i] <- 0
  }
  if(loc_type[i] != 0 & loc_type[i] != 4){
    #if start of a trip, increment x by one
    if(loc_type[i] == 1) {x <- x+1
    new_device <- FALSE}
    if(new_device == TRUE & (loc_type[i] != 0
                             & loc_type[i] != 1
                             & loc_type[i] != 2)) {
      x <- x+1
      new_device <- FALSE}
    
    #allocated value of x for trip_id for position 'i'.
    trip_id[i] <- x    
  }
}

# trip_id[1500:1600]
# # 
# test <- cbind(loc_type,trip_id, points_all$ring_number[!is.na(on_trip)])
# test[1550:1600,]
# test[loc_type == 4,]

# summary(loc_type == 4)
# summary(trip_id == 21)
# test <- points_all[loc_type == 4,]


# Give trip_id for all points (including invallid and dives)
trip_id_all <- rep(9999,length(on_trip))
trip_id_all[!is.na(on_trip)] <- trip_id
# summary(as.factor(trip_id_all))
# head(trip_id_all)

# summary(trip_id_all == 19)


n <- length(trip_id_all)
x <- 0
for(i in 1:n){
  if(trip_id_all[i] == 9999)  trip_id_all[i] <- x else{
    x <- trip_id_all[i]
  }
}

# summary(trip_id_all == 21)

# 124 'trips', not all neccessarilly foraging trips, will
# probably need to filter more.
# max(trip_id_all)




# length(trip_id_all)
# length(col.dist)
# length(points_all$date_time)

# Distribution of 'trip' start dates
# hist(points_all$date_time[loc_type == 1], breaks = "day",
#      freq = TRUE)
# ?hist
# length(loc_calc)
# Assemble table of GPS point info
# test <- cbind(trip_id_all, col.dist,
#               as.character(points_all$date_time),
#               as.character(points_all$type))
# 
# test2 <- test[1000:2000,]


# names(points_all)


# Output details to DB
# Trip classification
# Also output combined table of GPS points (for easier analysis)

# GPS details table -----
# - device_info_serial
# - date_time
# - trip_id
# - ring_number
gps_info <- data.frame(points_all$device_info_serial,
                       points_all$date_time,
                       points_all$ring_number,
                       trip_id_all,
                       col.dist)
# str(gps_info)
names(gps_info)  <-  c("device_info_serial",
                       "date_time",
                       "trip_id",
                       "col_dist_m")


# Assemble table of foraging trips -----
# - trip_id
# - start_time
# - end_time
# - device_id
# - ring_number

trip_ids <- unique(trip_id_all)[-1]
head(trip_ids)

# i <- 5

trip_start <- NULL
trip_end <- NULL
trip_id <- NULL
trip_device_info_serial <- NULL
trip_ring_number <- NULL
trip_start_vallid  <- NULL
trip_end_vallid   <- NULL
trip_col_dist <- NULL

# i <- 21
# summary(points_all$type == "bad_location")

# summary(trip_id_all == 19)

# tail(points_all$coldist)

# i <- 10
# summary(col.dist > 200)
for(i in 1:length(trip_ids)){
  id <- trip_ids[i]
  points.sub <- subset(points_all, trip_id_all == id)
  if(points.sub$device_type[1] == "uva"){
    trip.p <-  (points_all$coldist > 200)
  } else {trip.p <- (points.sub$type != "no_location") &
    (points.sub$type != "bad_location") &
    (points.sub$coldist > 200)}
  trip.p[is.na(trip.p)] <- FALSE
  points.sub.t <- points.sub[trip.p,]
  if(sum(1*(trip.p == TRUE)) < 1){
    trip_start_vallid[i] <- trip_end_vallid[i] <- NA
  } else {
    trip_start_vallid[i] <- min(points.sub.t$date_time)
    trip_end_vallid[i]   <- max(points.sub.t$date_time)
  }
  trip_start[i] <- min(points.sub$date_time)
  trip_end[i]   <- max(points.sub$date_time)
  trip_id[i]    <- id
  trip_col_dist[i]  <- max(points.sub$coldist, na.rm = TRUE)
  trip_device_info_serial[i] <- as.character(
    points.sub$device_info_serial[1])
  trip_ring_number[i] <- as.character(points.sub$ring_number[1])
}

# warnings()

# points_all[points_all$date_time == 
#              as.POSIXct("2014-06-14 15:47:13",
#                         origin = "1970-01-01",
#                         tz = "UTC"),]

trip_start <- as.POSIXct(trip_start, origin = "1970-01-01",
                         tz = "UTC")
trip_end <- as.POSIXct(trip_end, origin = "1970-01-01",
                       tz = "UTC")
trip_start_vallid <- as.POSIXct(trip_start_vallid,
                                origin = "1970-01-01",
                                tz = "UTC")
trip_end_vallid <- as.POSIXct(trip_end_vallid,
                              origin = "1970-01-01",
                              tz = "UTC")
trip_device_info_serial <- as.factor(trip_device_info_serial)
trip_ring_number <- as.factor(trip_ring_number)


trips.df <- data.frame(trip_id,
                       trip_device_info_serial,
                       trip_start,
                       trip_end,
                       trip_start_vallid,
                       trip_end_vallid,
                       trip_ring_number,
                       trip_col_dist)

str(trips.df)

names(trips.df) <- c("trip_id",
                     "device_info_serial",
                     "start_time",
                     "end_time",
                     "start_time_vallid",
                     "end_time_vallid",
                     "ring_number",
                     "col_dist_max")



# Summary statistics ----

trip_duration <- as.numeric(trips.df$end_time - trips.df$start_time)
hist(trip_duration)

summary(trip_duration < 30*60)
hist((trip_duration[trip_duration < 24*60*60]/(60*60)), breaks = 40)
hist((trip_duration[trip_duration < 24*60*60]/(60*60)), breaks = 40)


hist(trips.df$col_dist_max[trips.df$col_dist_max < 100000]/1000, breaks = 80)

# Exclude trips < 3 km (3000 m)
trips.f <- trips.df[trips.df$col_dist_max > 3000,]


trips.f$duration <- trip_duration[trips.df$col_dist_max > 3000]

dist_2009_km <- (trips.f$col_dist_max/1000)
dur_2009_km <- (trips.f$duration/(60*60))

png("guillemots_2014_dist_dur_comp.png", width = 800, height = 800, res = 200)
plot(dist_2009_km ~ dur_2009_km,
     #      xlim = c(0,50), ylim = c(0,70),
     log = "xy",
     ylab = "Distance (km)",
     xlab = "Duration (h)",
     las = 1
)
dev.off()
f <- dur_2009_km < 30
abline(lm(log(dist_2009_km[f]) ~ log(dur_2009_km[f])))



xs <- range(log(dur_2009_km[f]))
lmObj <- lm(log(dist_2009_km[f]) ~ log(dur_2009_km[f]))
ys.new <- predict(lmObj,  newdata = data.frame(x = xs))
lines(log(dur_2009_km[f]), ys.new, col = "black" ,lty=1,lwd=2)


png("guillemots_2014_dist_dur.png", width = 1600, height = 800, res = 200)
par(mfrow = c(1,2))
par(mar= c(5, 4, 4, 2) + 0.1)
# dev.off()
hist((trips.f$duration/(60*60)), breaks = 80,
     xlim = c(0,80),
     col = "grey",
     las = 1,
     xlab = "Trip duration (h)",
     main = "")
box()

# str(trips.f)

hist(trips.f$col_dist_max/1000, breaks = 20,
     col = "grey",
     las = 1,
     xlab = "Trip distance (km)",
     main = "")
box()
dev.off()

median(trips.f$col_dist_max/1000)
median((trips.f$duration/(60*60)))



hist(trips.f$duration/(60*60), xlim = c(0,100), breaks = 80)

times <- format(trips.f$start_time, format="%H:%M:%S")
dtTime <- as.numeric(trips.f$start_time - trunc(trips.f$start_time, "days"))

col.dif <- rainbow(15)

str(times)
plot((dtTime/60),(trips.f$duration/(60*60)),
     ylim = c(0,80),
     col = col.dif[as.numeric(trips.f$ring_number)])
abline(h = (24), lty = 2, lwd = 2)

unique(trips.f$ring_number[trips.f$duration > (24*60*60)])



# length(dtTime)
# time.dif <- difftime(trips.df$end_time, trips.df$start_time,
#                      units = "mins")
# 
# hist(as.numeric(time.dif)  )

# Output both tables to the database -----