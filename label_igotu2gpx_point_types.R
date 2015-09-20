# Primarily developed by Tom Evans at Lund University: tom.evans@biol.lu.se
# You are welcome to use parts of this code, but please give credit when using it extensively.

# This script is to produce a new database table to
# annotate the GPS fix data from the IGU loggers
# labelling points by whether they are vallid
# GPS fixes, plus weather they include diving etc.

# Read in DB data -----
# DB package
library("RODBC")

# Connect to DB
gps.db <- odbcConnectAccess2007('D:/Dropbox/tracking_db/murre_db/murre_db.accdb')

# Get GPS data
gpspoints <- sqlQuery(gps.db,
                   query = "SELECT g.device_info_serial, g.date_time, g.latitude, g.longitude, g.speed_ms, g.timeout, g.ehpe, g.sat_n, guillemots_track_session.ring_number
FROM guillemots_track_session, guillemots_gps_points_igu AS g
                   WHERE (((g.date_time)>=[guillemots_track_session].[start_date] And (g.date_time)<=[guillemots_track_session].[end_date]) AND ((guillemots_track_session.device_info_serial)=g.device_info_serial))
                   ORDER BY g.device_info_serial, g.date_time;",
                   as.is = TRUE)


# Label by whether vallid GPS fix -----

# Has location?
gps_val <- (gpspoints$long != 0) & (gpspoints$lat != 0)
summary(gps_val)

# Is good quality GPS fix?
f0 <- gps_val & (gpspoints$timeout <= 100)
f1 <- gps_val & (gpspoints$ehpe <= 200)
# Looks to be that these points shouldn't be excluded, though they are odd, as have zero satellites!
f2 <- ((gpspoints$ehpe >= 200) & gps_val & 
         (gpspoints$timeout != 250)  & (gpspoints$sat_n == 0))
gps_ok <- (f0 & f1) | f2
summary(gps_ok)

# Look at those flagged as NA - appear to be only the 2009 data
# which were not downloaded with the igotu2gpx program
gpspoints2 <- gpspoints[is.na(gps_ok),]
unique(gpspoints2$device_info_serial)


# 2009 points, whichc were downloaded with @trip, thus don't have
# the detailed information will be classified as ok
gps_ok[is.na(gps_ok)] <- TRUE
# 

# points.ehpe.bad2 <- gpspoints[f2,]
# points.ehpe.bad2 <- points.ehpe.bad[points.ehpe.bad$sat_n == 0,]


# hist(gpspoints$ehpe, breaks = 100)




# Label by 'behaviour' ------
#* 1. Flight
#* 2. Colony
#* 3. Diving (apparent)
#* 4. Water surface (swimming etc)
#* 5. Other (i.e. uncategorised)

# 1 . Flight ----
# label as flight if instantaneous speed exceeds
# 5 ms-1
flight <- gpspoints$speed_ms > 5
summary(flight)

# # Check above looks sensible
# hist(gpspoints$speed_ms, ylim = c(0,2000), breaks = 100)
# abline(v=5)

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
col.dist <- mapply(fun.dist,gpspoints$lat,gpspoints$long)

# Convert distance from km to m
col.dist <- col.dist * 1000

# Locations within 200 m of central location in colony
col.loc <- col.dist < 200
summary(col.loc)

# 
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


# 3. Diving (apparent) ----
diving <- ((gpspoints$lat == 0 ) | (gpspoints$long == 0 ) )& (gpspoints$timeout == 12)
summary(diving)

# # View number of diving fixes by hour of day
# hours <- as.POSIXlt(gpspoints$date_time[diving],
#                     tz = "UTC")
# hours.loc <- as.POSIXlt(hours, tz = "Europe/Stockholm")
# 
# 
# hist(((as.numeric(as.POSIXlt(hours.loc)$hour)+2) %%24), xlim = c(0,24), breaks = 24,
#      xlab = "Hour of day (UTC)",
#      ylab = "Diving GPS fixes",
#      main = "",
#      col = "grey60")


# Some labelling of previous/ next points? -----
# Could be good for example to label if previous point was diving
# so as to plot points according to this.
# When plotting could get average/ intermediate position for diving
# events

# Label points where next fix is diving
# Index thing
n <- length(gpspoints$lat)
# Make an index
ind <- c(1:n)

# Diving points
ind.dive <- ind[diving]

# Previous point
dive.prev.point <- ind.dive -1

# True/ false
dive.next <- rep(FALSE,n)
dive.next[dive.prev.point] <- TRUE


# Next points where previous point is diving
# Next point
dive.next.point <- ind.dive +1

# True/ false
dive.prev <- rep(FALSE,n)
dive.prev[dive.next.point] <- TRUE
# summary(dive.next)



# Combine information to label points ----
type <- rep("unclassified", n) 

# No location
# diving
type[diving] <- "dive"

# no loc, not diving
summary(!gps_val)
summary(diving)
points.test <- gpspoints[(!gps_val & !diving),]
type[(!gps_val & !diving)] <- "no_location"

# get 'map.trip' function
source("map.trip.R")

# View these points on map
# map.trip(points = gpspoints[gps_ok,], xlim = c(17.7,18.1),
         # ylim = c(57.2, 57.4))
map.trip(points = gpspoints[gps_ok,])

#missing points, not diving
ind.miss <- ind[(!gps_val & !diving)]

# Previous point
ind.miss.prev <- ind.miss -1

points2plot <- ind.miss.prev[gpspoints$long[ind.miss.prev] != 0]

points(gpspoints$lat[points2plot]~gpspoints$long[points2plot],
       col = "green", pch = 8)
# Appear to be nearly universally colony based locations.
# Removing these then won't matter too much - not parts of
# foraging trips.

summary(as.factor(type))

# With a location
# With a location but bad
bad_loc <- (gps_val & !gps_ok)
points(gpspoints$lat[bad_loc]~gpspoints$long[bad_loc],
       col = "magenta", pch = 8)
type[bad_loc] <- "bad_location"

# Flight
flight.points <- flight & !bad_loc
summary(flight.points)
type[flight.points] <- "flight"

summary(as.factor(type))


# Colony
# Could exclude bad locations here - but really they are probably colony if located there
# col.points <- col.loc & !bad_loc
col.points <- col.loc
summary(col.points)

type[col.points] <- "colony"
summary(as.factor(type))


# Map these points
points(gpspoints$lat[col.points]~gpspoints$long[col.points],
       col = "orange", pch = 8)

points(gpspoints$lat[type == "unclassified"]~gpspoints$long[type == "unclassified"],
       col = "light blue")

# Two obvious outlier points
type[gpspoints$lat > 57.7] <- "bad_location"

# points(points.ehpe.bad2$longitude, points.ehpe.bad2$latitude, col = "orange",
# cex = 5, pch= 8)
# 
# x <- points.ehpe.bad2$timeout == 250
# points(points.ehpe.bad2$longitude[x], points.ehpe.bad2$latitude[x], col = "black",
#        cex = 5, pch= 8)

# Remaining points - both water surface and pre/ post deployment
# points - will have to remove those later (by deployment period)
# These points will be 'unclassified'
# Later can classify into 'surface' and just filter out the 
# non-deployment points

type[type == "unclassified"] <- "surface"


# # 2009 points
# gpspoints3 <- cbind(gpspoints, type)
# gpspoints2 <- gpspoints3[is.na(gps_ok),]
# unique(gpspoints2$device_info_serial)
# unique(gpspoints2$type)



# Output to new DB table ----

# Assemble vairables to new data frame
export.table <- cbind(gpspoints$device_info_serial,
                      gpspoints$date_time)

# Add colony distance
export.table <- cbind(export.table, col.dist)

# dive next and previous
export.table <- cbind(export.table, dive.prev, dive.next)

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

# Check for duplicates
test <- cbind(export.table$device_info_serial, export.table$date_time)
test2 <- duplicated(test)

# no duplicates
export.table.new <- export.table[!test2,]


# Write to database


#will be neccessary to edit table in Access after to define data-types and primary keys and provide descriptions for each variable.
sqlSave(gps.db, export.table.new,
        tablename = "guillemots_gps_points_igu_class",
        append = FALSE, rownames = FALSE, colnames = FALSE,
        verbose = FALSE, safer = TRUE, addPK = FALSE, fast = TRUE,
        test = FALSE, nastring = NULL,
        varTypes =  c(date_time = "datetime")
)
