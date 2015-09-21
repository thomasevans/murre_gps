









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