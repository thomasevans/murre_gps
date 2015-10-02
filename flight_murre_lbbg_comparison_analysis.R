

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
                   query = "SELECT guillemots_gps_flights.*, guillemots_gps_flights_weather_components_3.va_mean, guillemots_gps_flights_weather_components_3.va_median, guillemots_gps_flights_weather_components_3.va_max, guillemots_gps_flights_weather_components_3.va_min, guillemots_gps_flights_weather_components_3.wind_side_mean, guillemots_gps_flights_weather_components_3.wind_side_median, guillemots_gps_flights_weather_components_3.wind_tail_mean, guillemots_gps_flights_weather_components_3.wind_tail_median, guillemots_gps_flights_weather_components_3.wind_side_10_mean, guillemots_gps_flights_weather_components_3.wind_side_10_median, guillemots_gps_flights_weather_components_3.wind_tail_10_mean, guillemots_gps_flights_weather_components_3.wind_tail_10_median, guillemots_gps_flights_weather_components_3.cloud_tot_mean,
guillemots_gps_flights_weather_components_3.ecmwf_pressure_sea_lev_mean, guillemots_gps_flights_weather_components_3.temp_k_mean, guillemots_gps_flights_weather_components_3.wind_10m_mean, guillemots_gps_flights_weather_components_3.wind_dir_mean, guillemots_gps_flights_weather_components_3.wind_10m_flt_ht_mean, guillemots_gps_flights_weather_components_3.wind_10m_flt_ht_median
                   FROM guillemots_gps_flights INNER JOIN guillemots_gps_flights_weather_components_3 ON guillemots_gps_flights.flight_id = guillemots_gps_flights_weather_components_3.flight_id;",
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


lbbg.sub <- cbind.data.frame(lbbg$flight_id,
                             lbbg$start_time,
                             lbbg$duration,
                             lbbg$device_info_serial,
                             lbbg$dist_a_b,
                             lbbg$alt_med,
                             lbbg$wind_side_mean,
                             lbbg$wind_side_mean_10,
                             lbbg$wind_head_tail_mean,
                             lbbg$wind_head_tail_mean_10,
                             lbbg$ground_speed_median,
                             lbbg$head_speed_mean,
                             lbbg$head_speed_median,
                             lbbg$cloud_cover_totalmean,
                             lbbg$temperature_2mmean,
                             lbbg$sea_level_pressuremean,
                             lbbg$species)

murre.sub <- cbind.data.frame(murres$flight_id,
                              murres$start_time,
                              murres$duration,
                              murres$device_info_serial,
                              murres$dist_straight,
                              murres$alt_median,
                              murres$wind_side_mean,
                              murres$wind_side_10_mean,
                              murres$wind_tail_mean,
                              murres$wind_tail_10_mean,
                              murres$speed_median,
                              murres$va_mean,
                              murres$va_median,
                              murres$cloud_tot_mean,
                              murres$temp_k_mean,
                              murres$ecmwf_pressure_sea_lev_mean,
                              murres$species)

names(lbbg.sub) <- names(murre.sub)
flights.all <- rbind.data.frame(lbbg.sub,murre.sub)
names(flights.all) <- c("flight_id",
                        "start_time",
                        "duration",
                        "device_info_serial",
                        "dist_straight",
                        "alt_median",
                        "wind_side_mean",
                        "wind_side_10_mean",
                        "wind_tail_mean",
                        "wind_tail_10_mean",
                        "speed_median",
                        "va_mean",
                        "va_median",
                        "cloud_tot_mean",
                        "temp_k_mean",
                        "ecmwf_pressure_sea_lev_mean",
                        "species")


# Add head/ tail wind dummy variable
sign.wind <- sign(flights.all$wind_tail_10_mean)
wind.type <- (as.factor(sign.wind))
summary(wind.type)


flights.all <- cbind.data.frame(flights.all,wind.type)




# Plotting -------
library("ggplot2")


# Plot data for va vs. head-tail wind ----------

# Sub-set for altitude analysis
# Remove NAs and extreme outliers >150 or <-50
flights.all.alt <- flights.all[(
  !is.na(flights.all$alt_median) &
    flights.all$alt_median > -50 &
    flights.all$alt_median < 150 &
    !is.na(flights.all$wind.type)
),]


resa = 72*4
png("alt_head_tail_ggplot.png", res = resa, width = 8*resa, height = 6*resa)
# ?png
ggplot(flights.all.alt, aes(wind_tail_10_mean, alt_median, color = factor(species), 
                # shape = factor(year), 
                linetype = factor(wind.type)))+
  geom_point(alpha = 60/100, colour="white", size = 2) +
  geom_point(alpha = 70/100, size = 1.5)+
  scale_colour_manual(values=c("#3182bd", "#c51b8a"))+
  stat_smooth(method = "glm", size = 1.5)+
  # geom_line(size = 2)+
  scale_x_continuous(limits = c(-10, 10))+
  scale_y_continuous(limits = c(-25, 100)) +
  theme(legend.position="none") +
  theme(axis.text=element_text(size=18),
         axis.title=element_text(size=20,face="bold")) +
  labs(x = expression(paste("Wind speed (ms",""^{-1}, ")")), y = paste("Flight altitude (m)"))
# ?stat_smooth
dev.off()



# Analysis for va vs. head-tail wind -------




# Plot data for alt vs. head-tail wind -------

# Sub-set for va analysis
# Remove NAs and extreme outliers >150 or <-50
flights.all.va <- flights.all[(
  !is.na(flights.all$va_median) &
    flights.all$va_median < 40 &
    !is.na(flights.all$wind.type)
),]


resa = 72*4
png("va_head_tail_ggplot.png", res = resa, width = 8*resa, height = 6*resa)
# ?png
ggplot(flights.all.va, aes(wind_tail_10_mean, va_median, color = factor(species), 
                            # shape = factor(year), 
                            linetype = factor(wind.type)))+
  geom_point(alpha = 60/100, colour="white", size = 2) +
  geom_point(alpha = 70/100, size = 1.5)+
  scale_colour_manual(values=c("#3182bd", "#c51b8a"))+
  stat_smooth(method = "glm", size = 1.5)+
  # geom_line(size = 2)+
  scale_x_continuous(limits = c(-10, 10))+
  scale_y_continuous(limits = c(5, 20)) +
  theme(legend.position="none") +
  theme(axis.text=element_text(size=18),
        axis.title=element_text(size=20,face="bold")) +
  labs(x = expression(paste("Wind speed (ms",""^{-1}, ")")),
       y = expression(paste("Air speed (ms",""^{-1}, ")")))
# ?stat_smooth
dev.off()





# Analysis for alt vs. head-tail wind ------



