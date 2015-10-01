

# Stats for va and alt in murre and lbbg

# ** Get data for both species ------
library("RODBC")

# Get lbbg data ----
# Establish a connection to the database
gps.db2 <- odbcConnectAccess2007('D:/Dropbox/tracking_db/GPS_db.accdb')

# See what tables are available
#sqlTables(gps.db)


lbbg <- sqlQuery(gps.db2, query="SELECT DISTINCT f.*
                 FROM lund_flight_com_lbbg AS f
                 ORDER BY f.device_info_serial ASC, f.start_time ASC;")



# Get murre data ----
gps.db <- odbcConnectAccess2007('D:/Dropbox/tracking_db/murre_db/murre_db.accdb')


# Get GPS data
murres <- sqlQuery(gps.db,
                   query = "SELECT guillemots_gps_flights.*, guillemots_gps_flights_weather_components_2.va_mean, guillemots_gps_flights_weather_components_2.va_median, guillemots_gps_flights_weather_components_2.va_max, guillemots_gps_flights_weather_components_2.va_min, guillemots_gps_flights_weather_components_2.wind_side_mean, guillemots_gps_flights_weather_components_2.wind_side_median, guillemots_gps_flights_weather_components_2.wind_tail_mean, guillemots_gps_flights_weather_components_2.wind_tail_median, guillemots_gps_flights_weather_components_2.cloud_tot_mean, guillemots_gps_flights_weather_components_2.temp_k_mean, guillemots_gps_flights_weather_components_2.wind_10m_mean, guillemots_gps_flights_weather_components_2.wind_dir_mean, guillemots_gps_flights_weather_components_2.wind_10m_flt_ht_mean, guillemots_gps_flights_weather_components_2.wind_10m_flt_ht_median
                   FROM guillemots_gps_flights INNER JOIN guillemots_gps_flights_weather_components_2 ON guillemots_gps_flights.flight_id = guillemots_gps_flights_weather_components_2.flight_id;
                   ",
                   as.is = TRUE)


# Filter this data

f_dist_col <- murres$col_dist_dif < -1000
f_final <- murres$type == "final"
f_va_alt <- !is.na(murres$va_mean) & !is.na(murres$alt_mean)
murre_f <- f_dist_col & f_final & f_va_alt
summary(murre_f)

murres_old <- murres

murres <- murres[murre_f,]



# *** Combine flight data into new table, with column for species ------
# Variables required:
#' Responce:
#' va
#' altitude
#' 
#' Fixed:
#' Wind head-tail
#' Wind side
#' distance - straight-line
#' cloud total
#' sea level pressure
#' temperature
#' 
#' Random:
#' individual

# add species columns
lbbg$species <- "lbbg"
murres$species <- "murre"




# Plot data for va vs. head-tail wind ----------




# Analysis for va vs. head-tail wind -------




# Plot data for alt vs. head-tail wind -------




# Analysis for alt vs. head-tail wind ------



