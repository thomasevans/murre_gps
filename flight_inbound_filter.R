

# Filter guillemot/ murre flights

# Check distributions etc to choose sensible filters


# Load data --------

# Read in DB data -----
# DB package
library("RODBC")

# Connect to DB
gps.db <- odbcConnectAccess2007('D:/Dropbox/tracking_db/murre_db/murre_db.accdb')

# Get GPS data
flights <- sqlQuery(gps.db,
                      query = "SELECT guillemots_gps_flights.*, guillemots_gps_flights_weather_components_2.va_mean, guillemots_gps_flights_weather_components_2.va_median, guillemots_gps_flights_weather_components_2.va_max, guillemots_gps_flights_weather_components_2.va_min, guillemots_gps_flights_weather_components_2.wind_side_mean, guillemots_gps_flights_weather_components_2.wind_side_median, guillemots_gps_flights_weather_components_2.wind_tail_mean, guillemots_gps_flights_weather_components_2.wind_tail_median, guillemots_gps_flights_weather_components_2.cloud_tot_mean, guillemots_gps_flights_weather_components_2.temp_k_mean, guillemots_gps_flights_weather_components_2.wind_10m_mean, guillemots_gps_flights_weather_components_2.wind_dir_mean, guillemots_gps_flights_weather_components_2.wind_10m_flt_ht_mean, guillemots_gps_flights_weather_components_2.wind_10m_flt_ht_median
FROM guillemots_gps_flights INNER JOIN guillemots_gps_flights_weather_components_2 ON guillemots_gps_flights.flight_id = guillemots_gps_flights_weather_components_2.flight_id;
                    ",
                      as.is = TRUE)





# Check distributions -----
# Type - final
f_final <- flights$type == "final"
summary(f_final)

# N points
hist(flights$n_points[f_final])
hist(flights$n_points[f_final], breaks = 500, xlim = c(0,20))

# Distance travelled relative to colony
hist(flights$col_dist_dif[f_final])
hist(flights$col_dist_dif[f_final], xlim = c(-2000,500), breaks = 100)

f_dist_col <- flights$col_dist_dif < -1000
summary(f_dist_col)
# Only keep flights of at least 1 km in direction of colony
summary(f_dist_col & f_final)

# N points again (with new filters)
hist(flights$n_points[f_dist_col & f_final])
hist(flights$n_points[f_dist_col & f_final], breaks = 500, xlim = c(0,20))

f_n_points_5 <- (flights$n_points < 5)
summary(f_n_points_5)

# All filters now!
summary(f_n_points_5 & f_dist_col & f_final)


# Flights with info required
# va
# altitude
f_va_alt <- !is.na(flights$va_mean) & !is.na(flights$alt_mean)
summary(f_va_alt)
summary(f_n_points_5 & f_dist_col & f_final & f_va_alt)
# Only 36 flights now remaining!


# Combine filters
f_all <- f_n_points_5 & f_dist_col & f_final & f_va_alt
f_all_expoints <- f_dist_col & f_final & f_va_alt
summary(f_all_expoints)
summary(f_all)

# Inspect filtered data -----
hist(flights$va_median[f_all])
hist(flights$va_mean[f_all])
hist(flights$va_max[f_all])
hist(flights$va_median[f_all_expoints], breaks = 40)



hist(flights$alt_median, breaks = 1000, xlim = c(-100,100))
hist(flights$alt_median[f_all], breaks = 40)
hist(flights$alt_median[f_all_expoints], breaks = 40)









