# Primarily developed by Tom Evans at Lund University: tom.evans@biol.lu.se
# You are welcome to use parts of this code, but please give credit when using it extensively.

# This script is to produce a new database table to
# annotate the GPS fix data from the UVA loggers
# labelling points by whether they are vallid
# GPS fixes, plus weather they include flight etc.

# Read in DB data -----
# DB package
library("RODBC")

# Connect to DB
gps.db <- odbcConnectAccess2007('D:/Dropbox/tracking_db/murre_db/murre_db.accdb')

# Get GPS data
gpspoints <- sqlQuery(gps.db,
                      query = "SELECT uvabits_gps.device_info_serial, uvabits_gps.date_time, uvabits_gps.latitude, uvabits_gps.longitude, uvabits_gps.speed_2d, guillemots_track_session.ring_number
FROM guillemots_track_session, uvabits_gps
                      WHERE (((uvabits_gps.date_time)>=[guillemots_track_session].[start_date] And (uvabits_gps.date_time)<=[guillemots_track_session].[end_date]) AND ((uvabits_gps.latitude)<>0) AND ((uvabits_gps.longitude)<>0) AND ((guillemots_track_session.device_info_serial)=[uvabits_gps].[device_info_serial]))
                      ORDER BY uvabits_gps.device_info_serial, uvabits_gps.date_time;",
                      as.is = TRUE)


# Visualist the data
# get 'map.trip' function
source("map.trip.R")

# View these points on map
# map.trip(points = gpspoints[gps_ok,], xlim = c(17.7,18.1),
# ylim = c(57.2, 57.4))
map.trip(points = gpspoints)

# Remove extreme outlier point
gpspoints <- gpspoints[gpspoints$latitude < 59, ]
map.trip(points = gpspoints)


# Label by 'behaviour' ------
#* 1. Flight
#* 2. Colony
#* 3. Water surface (swimming etc)
#* 4. Other (i.e. uncategorised)??

# 1 . Flight ----
# label as flight if instantaneous speed exceeds
# 5 ms-1
flight <- gpspoints$speed_2d > 5
summary(flight)


# # # Check that above looks sensible
# hist(gpspoints$speed_2d, ylim = c(0,2000), xlim = c(0,100), breaks = 1000)
# abline(v = 5)

# 2. Collony ----

# For each GPS position get distance from colony
# If GPS location is 0,0 give NA
fun.dist <- function(lat,long){
  # Define collony location
  lat.c  <- 57.289848
  long.c <- 17.958252
  
  # Get function to calculate distances
  source("deg.dist.R")
  
  if(lat == 0 | long == 0) {x <- NA} else {
    x <- deg.dist(long.c,lat.c, long,lat) 
  }
  return(x)
}

# Get distance for each point
col.dist <- mapply(fun.dist,gpspoints$latitude,gpspoints$longitude)

# Convert distance from km to m
col.dist <- col.dist * 1000

# Locations within 100 m of central location in colony
col.loc <- col.dist < 100
# Relatively few points because of use of fence in settings
summary(col.loc)


# # Have a look at this data
# hist(col.dist)
# hist(col.dist, breaks = 40)
# hist(col.dist, breaks = 5000, xlim = c(0, 500))
# # Appears that distances between ca. 0 and 200
# # correspond to murre-lab 
# 
# # See where these locations falling between 100 - 200 m are located
# # First plot all locations within 500 m of colony:
# m500 <- col.dist < 500
# plot(gpspoints$longitude[m500], gpspoints$latitude[m500])
# 
# m100 <- col.dist < 100
# points(gpspoints$longitude[m100], gpspoints$latitude[m100], col = "grey")
# m100_200 <- (col.dist > 100) & (col.dist < 200)  
# points(gpspoints$longitude[m100_200], gpspoints$latitude[m100_200], col = "blue")
# # colony location
# points(17.958252, 57.289848, col = "red", cex = 5)




# NA points/ points to exclude??? -----
nf <- (is.na(gpspoints$speed_2d))

points(gpspoints$longitude[nf], gpspoints$latitude[nf],
       col = "green", pch = 8)
(is.na(gpspoints$speed_2d))


# A few points with very high speeds
x <- gpspoints$speed_2d > 40
summary(x)


bad_loc <- nf | x

map.trip(points = gpspoints)
points(gpspoints$latitude[bad_loc]~gpspoints$longitude[bad_loc],
       col = "magenta", pch = 8)




# Combine information to label points ----
n <- length(gpspoints$latitude)
type <- rep("unclassified", n) 

# colony
type[col.loc] <- "colony"
summary(as.factor(type))


# flight
type[flight] <- "flight"
summary(as.factor(type))

# bad
type[bad_loc] <- "bad_location"
summary(as.factor(type))

# rest as surface
type[type == "unclassified"] <- "surface"
summary(as.factor(type))



# Output to new DB table ----

# Assemble vairables to new data frame
export.table <- cbind(gpspoints$device_info_serial,
                      gpspoints$date_time)

# Add colony distance
export.table <- cbind(export.table, col.dist)

# add points classification
export.table <- cbind(export.table, type)

# Format as data frame
export.table <- as.data.frame(export.table)
str(export.table)

# Change structure
str(col.dist)
export.table$col.dist <- col.dist


names(export.table)[1] <- "device_info_serial"
names(export.table)[2] <- "date_time"

export.table$date_time <- as.POSIXct(export.table$date_time, tz = "UTC")

str(export.table)

# Output annotation to new DB table include device_info_serial
# and date_time for primary key data

# Write to database


#will be neccessary to edit table in Access after to define data-types and primary keys and provide descriptions for each variable.
sqlSave(gps.db, export.table,
        tablename = "guillemots_gps_points_uva_class",
        append = FALSE, rownames = FALSE, colnames = FALSE,
        verbose = FALSE, safer = TRUE, addPK = FALSE, fast = TRUE,
        test = FALSE, nastring = NULL,
        varTypes =  c(date_time = "datetime")
)


