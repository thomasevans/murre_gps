


# Read in GPS data including derived wind components, direction (track bearing), etc








# Flight vector components ------------------------

#' Calculating heading vector (we already have the wind vector
#' and the track vector).
#' In principle this is simple vector addition.
#' 

# Calculate heading vectors from speed and direction

veast
vnorth


# Calculating u and v component of heading vector
# Subtract wind vector from ground vector, leaving
# heading vector
# names(points)
# names(points.weather)
head_u_ecmwf <- veast  - gps.data$wind_u_10m_flt_ht_ecmwf
head_v_ecmwf <- vnorth - gps.data$wind_v_10m_flt_ht_ecmwf

source("wind_dir_speed.R")


head.info <- t(mapply(wind.dir.speed, head_u_ecmwf,
                      head_v_ecmwf))
# names(points.weather)
# Make dataframe
head.info <- as.data.frame(cbind(head.info, head_u_ecmwf, head_v_ecmwf))

# Give names to columns
names(head.info) <- c("head_speed_ecmwf", "head_dir_ecmwf", "head_u_ecmwf", "head_v_ecmwf")

# Add to main dataframe
gps.data.par <- cbind(gps.data.par, head.info)

# Ground speed + track heading
ground_speed <- t(mapply(wind.dir.speed, gps.data$veast,
                         gps.data$vnorth))
ground_heading <- ground_speed[,2]
ground_speed   <- ground_speed[,1]

# Add to main dataframe
gps.data.par <- cbind(gps.data.par, ground_speed, ground_heading)


# median(ground_speed[ground_speed < 50], na.rm = TRUE)
# median(gps.data.par$head_speed[gps.data.par$head_speed < 50], na.rm = TRUE)

# Heading track vs. ground heading, 0 - 180 indicates 'drift' to right
# Values 180 - 360 indicate 'drift' to left.
# hist((gps.data.par$ground_heading - gps.data.par$head_dir) %% 360)

names(gps.data.par) <- c("flight_id", "device_info_serial",
                         "date_time", "wind_u_10m_ecmwf",
                         "wind_v_10m_ecmwf",
                         "wind_u_10m_flt_ht_ecmwf",
                         "wind_v_10m_flt_ht_ecmwf",
                         "wind_speed_flt_ht_ecmwf",
                         "wind_dir_ecmwf", "wind_speed_10m_ecmwf",
                         "wind_origin_ecmwf", "head_speed_ecmwf",
                         "head_dir_ecmwf", "head_u_ecmwf",
                         "head_v_ecmwf", "ground_speed",
                         "ground_heading")


