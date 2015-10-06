# Checking distributions of va, vg, and altitude - comparing flight summaries and GPS data




# Get data -----

# Read in DB data
# DB package
library("RODBC")

# Connect to murre DB
gps.db <- odbcConnectAccess2007('D:/Dropbox/tracking_db/murre_db/murre_db.accdb')

# Establish a connection to the lbbg database
gps.db2 <- odbcConnectAccess2007('D:/Dropbox/tracking_db/GPS_db.accdb')


# Get flight info - murre

# Get GPS data
flights_murre <- sqlQuery(gps.db,
                          query = "SELECT guillemots_gps_flights.*, guillemots_gps_flights_weather_components_2.va_mean, guillemots_gps_flights_weather_components_2.va_median, guillemots_gps_flights_weather_components_2.va_max, guillemots_gps_flights_weather_components_2.va_min, guillemots_gps_flights_weather_components_2.wind_side_mean, guillemots_gps_flights_weather_components_2.wind_side_median, guillemots_gps_flights_weather_components_2.wind_tail_mean, guillemots_gps_flights_weather_components_2.wind_tail_median, guillemots_gps_flights_weather_components_2.cloud_tot_mean, guillemots_gps_flights_weather_components_2.temp_k_mean, guillemots_gps_flights_weather_components_2.wind_10m_mean, guillemots_gps_flights_weather_components_2.wind_dir_mean, guillemots_gps_flights_weather_components_2.wind_10m_flt_ht_mean, guillemots_gps_flights_weather_components_2.wind_10m_flt_ht_median
                          FROM guillemots_gps_flights INNER JOIN guillemots_gps_flights_weather_components_2 ON guillemots_gps_flights.flight_id = guillemots_gps_flights_weather_components_2.flight_id;
                          ",
                          as.is = TRUE)

# Get flight info - lbbg
flights_lbbg <- sqlQuery(gps.db2, query="SELECT DISTINCT f.*
                         FROM lund_flight_com_lbbg AS f
                         ORDER BY f.device_info_serial ASC, f.start_time ASC;")


# Get flight GPS locations - lbbg
gps_lbbg <- sqlQuery(gps.db2, query="SELECT gps_uva_tracking_speed_3d_limited.device_info_serial, gps_uva_tracking_speed_3d_limited.date_time, gps_uva_tracking_speed_3d_limited.latitude, gps_uva_tracking_speed_3d_limited.longitude, lund_flight_com_lbbg.flight_id, gps_uva_tracking_speed_3d_limited.speed_3d
FROM gps_uva_tracking_speed_3d_limited, lund_flight_com_lbbg
WHERE (((gps_uva_tracking_speed_3d_limited.date_time)>=[lund_flight_com_lbbg].[start_time] And (gps_uva_tracking_speed_3d_limited.date_time)<=[lund_flight_com_lbbg].[end_time]) AND ((gps_uva_tracking_speed_3d_limited.latitude)<>0) AND ((gps_uva_tracking_speed_3d_limited.longitude)<>0) AND ((lund_flight_com_lbbg.device_info_serial)=[gps_uva_tracking_speed_3d_limited].[device_info_serial]))
ORDER BY gps_uva_tracking_speed_3d_limited.device_info_serial, gps_uva_tracking_speed_3d_limited.date_time;")



# Get flight GPS locations - murre
gps_murre <- sqlQuery(gps.db,
                      query = "SELECT * FROM 
                      (SELECT guillemots_gps_points_igu.device_info_serial, guillemots_gps_points_igu.date_time, guillemots_gps_points_igu.latitude, guillemots_gps_points_igu.longitude, guillemots_gps_flights.flight_id, guillemots_gps_flights.deploy_id, guillemots_gps_points_components_wind.ground_speed, guillemots_gps_points_components_wind.head_speed_ecmwf
                      FROM guillemots_gps_flights, guillemots_gps_points_igu INNER JOIN guillemots_gps_points_components_wind ON (guillemots_gps_points_igu.device_info_serial = guillemots_gps_points_components_wind.device_info_serial) AND (guillemots_gps_points_igu.date_time = guillemots_gps_points_components_wind.date_time)
                      WHERE (((guillemots_gps_points_igu.date_time)>=[guillemots_gps_flights].[start_time] And (guillemots_gps_points_igu.date_time)<=[guillemots_gps_flights].[end_time]) AND ((guillemots_gps_points_igu.latitude)<>0) AND ((guillemots_gps_points_igu.longitude)<>0))
                      ORDER BY guillemots_gps_points_igu.device_info_serial, guillemots_gps_points_igu.date_time)
                      UNION SELECT * FROM 
                      (SELECT uvabits_gps.device_info_serial, uvabits_gps.date_time, uvabits_gps.latitude, uvabits_gps.longitude, guillemots_gps_flights.flight_id, guillemots_gps_flights.deploy_id, guillemots_gps_points_components_wind.ground_speed, guillemots_gps_points_components_wind.head_speed_ecmwf
                      FROM guillemots_gps_flights, uvabits_gps INNER JOIN guillemots_gps_points_components_wind ON (guillemots_gps_points_components_wind.date_time = uvabits_gps.date_time) AND (uvabits_gps.device_info_serial = guillemots_gps_points_components_wind.device_info_serial)
                      WHERE (((uvabits_gps.date_time)>=[guillemots_gps_flights].[start_time] And (uvabits_gps.date_time)<=[guillemots_gps_flights].[end_time]) AND ((uvabits_gps.latitude)<>0) AND ((uvabits_gps.longitude)<>0) AND ((guillemots_gps_flights.device_info_serial)=[uvabits_gps].[device_info_serial]))
                      ORDER BY uvabits_gps.device_info_serial, uvabits_gps.date_time);",
                          as.is = TRUE)




# Filter murre flights -------
f_dist_col <- flights_murre$col_dist_dif < -1000
f_final <- flights_murre$type == "final"
f_va_alt <- !is.na(flights_murre$va_mean) & !is.na(flights_murre$alt_mean)
f_all_expoints <- f_dist_col & f_final & f_va_alt
summary(f_all_expoints)


flights_murre.f <- flights_murre[f_all_expoints,]
gps_murre.f <- gps_murre[gps_murre$flight_id %in% flights_murre.f$flight_id,]


# Plot histograms of Va + Vg --------

par(mfrow = (c(2,2)))

# vg
# llbg - points
hist(gps_lbbg$speed_3d, xlim = c(1,26), breaks = 40)


# murre - points
hist(gps_murre.f$ground_speed, xlim = c(1,26), breaks = 40)

# lbbg - flights
hist(flights_lbbg$speed_inst_med, xlim = c(1,26), breaks = 40)

# murre - flights
hist(flights_murre.f$speed_median, xlim = c(1,26), breaks = 40)


