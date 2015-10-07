

# Stats for va and alt in murre and lbbg

# *** Get data for both species ------
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
                   query = "SELECT guillemots_gps_flights.*, guillemots_gps_flights_weather_components_3.va_mean, guillemots_gps_flights_weather_components_3.va_median, guillemots_gps_flights_weather_components_3.va_max, guillemots_gps_flights_weather_components_3.va_min, guillemots_gps_flights_weather_components_3.wind_side_mean, guillemots_gps_flights_weather_components_3.wind_side_median, guillemots_gps_flights_weather_components_3.wind_tail_mean, guillemots_gps_flights_weather_components_3.wind_tail_median, guillemots_gps_flights_weather_components_3.wind_side_10_mean, guillemots_gps_flights_weather_components_3.wind_side_10_median, guillemots_gps_flights_weather_components_3.wind_tail_10_mean, guillemots_gps_flights_weather_components_3.wind_tail_10_median, guillemots_gps_flights_weather_components_3.cloud_tot_mean, guillemots_gps_flights_weather_components_3.ecmwf_pressure_sea_lev_mean, guillemots_gps_flights_weather_components_3.temp_k_mean, guillemots_gps_flights_weather_components_3.wind_10m_mean, guillemots_gps_flights_weather_components_3.wind_dir_mean, guillemots_gps_flights_weather_components_3.wind_10m_flt_ht_mean, guillemots_gps_flights_weather_components_3.wind_10m_flt_ht_median
FROM guillemots_gps_flights INNER JOIN guillemots_gps_flights_weather_components_3 ON guillemots_gps_flights.flight_id = guillemots_gps_flights_weather_components_3.flight_id;
                   ",
                   as.is = TRUE)


# Filter this data
f_dist_col <- murres$col_dist_dif < -1000
f_final <- murres$type == "final"
f_va_alt <- !is.na(murres$va_mean) & !is.na(murres$alt_mean)
f_move <- murres$dist_straight > 3500
murre_f <- f_dist_col & f_final & f_va_alt & f_move



# f_dist_col <- murres$col_dist_dif < -1000
# f_final <- murres$type == "final"
# f_va_alt <- !is.na(murres$va_mean) & !is.na(murres$alt_mean)
# murre_f <- f_dist_col & f_final & f_va_alt
# summary(murre_f)

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
                             lbbg$species,
                             lbbg$device_info_serial)

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
                              murres$species,
                              murres$ring_number)

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
                        "species",
                        "ring_number")


# Add head/ tail wind dummy variable
sign.wind <- sign(flights.all$wind_tail_10_mean)
wind.type <- (as.factor(sign.wind))
summary(wind.type)


flights.all <- cbind.data.frame(flights.all,wind.type)


levels(flights.all$wind.type) <- c("head", "tail")
summary(flights.all$wind.type)

flights.all <- flights.all[!is.na(flights.all$wind.type),]

# *** Plotting -------
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

hist(flights.all.va$wind_tail_10_mean[flights.all.va$species == "murre"])
hist(flights.all.va$wind_tail_10_mean[flights.all.va$species == "lbbg"])

# *** Analysis of Va and Altitude -------
install.packages(c("lme4","arm","MuMIn"))

library(lme4)
library(arm)

# stdz.model<-standardize(mod.1, standardize.y= FALSE)
# summary(stdz.model)

library(MuMIn)


# Analysis for va vs. head-tail wind -------

# LBBG
va.lbbg <- flights.all.va[flights.all.va$species == "lbbg",]
str(va.lbbg)
va.lbbg$wind_tail_10_mean_abs <- abs(va.lbbg$wind_tail_10_mean)
va.lbbg$wind_side_10_mean_abs <- abs(va.lbbg$wind_side_10_mean)
levels(va.lbbg$wind.type)
mod.va.lbbg <- glmer(va_median ~ 
                       # wind_side_10_mean_abs +
                       wind_tail_10_mean_abs * wind.type +
                # dist_straight +
                # cloud_tot_mean +
                # temp_k_mean +
                # ecmwf_pressure_sea_lev_mean +
                (1|ring_number),
              family = gaussian,
              data = va.lbbg)

mod.va.lbbg.std <-standardize(mod.va.lbbg, standardize.y = FALSE)

mod.va.lbbg.std <- mod.va.lbbg

summary(mod.va.lbbg.std)

r.squaredGLMM(mod.va.lbbg.std)

plot(mod.va.lbbg.std)

mod.va.lbbg.std.ci.Wald <- confint(mod.va.lbbg.std, method="Wald")
# warnings()
plot(mod.va.lbbg.std.ci.Wald)

# 0.3135936*-0.6168069
# 0.3135936*-0.3467022
# 0.5100054*-0.6168069
# 0.5100054*-0.3467022

library(lattice)
ci_dat <-mod.va.lbbg.std.ci.Wald
ci_dat <- cbind(ci_dat, mean=rowMeans(ci_dat))
ci_df <- data.frame(coef=row.names(ci_dat), ci_dat)
names(ci_df)[2:3] <- c('lwr', 'upr')


lattice::dotplot(coef ~ mean, ci_df,
                 xlim = c(-1,1),
                 #                  cexl.lab = 1.5, cex.axis = 1.5,
                 xlab = list("coef",cex=1.3),
                 panel = function(x, y) {
                   panel.segments(ci_df$lwr, y, ci_df$upr, y, lwd =2)
                   panel.xyplot(x, y, pch=18, cex = 1.2, col = "black")
                   panel.abline(v=0, lty=2)
                 },scales=list(y=list(cex=1.2), x = list(cex = 1.2))
)



# murre
va.murre <- flights.all.va[flights.all.va$species == "murre",]
str(va.murre)
va.murre$wind_tail_10_mean_abs <- abs(va.murre$wind_tail_10_mean)
va.murre$wind_side_10_mean_abs <- abs(va.murre$wind_side_10_mean)
levels(va.murre$wind.type)
mod.va.murre <- glmer(va_median ~ 
                       # wind_side_10_mean_abs +
                       wind_tail_10_mean_abs * wind.type +
                       # dist_straight +
                       # cloud_tot_mean +
                       # temp_k_mean +
                       # ecmwf_pressure_sea_lev_mean +
                       (1|ring_number),
                     family = gaussian,
                     data = va.murre)

mod.va.murre.std <-standardize(mod.va.murre, standardize.y = FALSE)

mod.va.murre.std <- mod.va.murre

summary(mod.va.murre.std)

r.squaredGLMM(mod.va.murre.std)

mod.va.murre.std.ci.Wald <- confint(mod.va.murre.std, method="Wald")
# warnings()
plot(mod.va.murre.std.ci.Wald)


library(lattice)
ci_dat <-mod.va.murre.std.ci.Wald
ci_dat <- cbind(ci_dat, mean=rowMeans(ci_dat))
ci_df <- data.frame(coef=row.names(ci_dat), ci_dat)
names(ci_df)[2:3] <- c('lwr', 'upr')


lattice::dotplot(coef ~ mean, ci_df,
                 xlim = c(-2,2),
                 #                  cexl.lab = 1.5, cex.axis = 1.5,
                 xlab = list("coef",cex=1.3),
                 panel = function(x, y) {
                   panel.segments(ci_df$lwr, y, ci_df$upr, y, lwd =2)
                   panel.xyplot(x, y, pch=18, cex = 1.2, col = "black")
                   panel.abline(v=0, lty=2)
                 },scales=list(y=list(cex=1.2), x = list(cex = 1.2))
)



# Analysis for alt vs. head-tail wind ------

#lbbg

# LBBG
alt.lbbg <- flights.all.alt[flights.all.va$species == "lbbg",]
str(alt.lbbg)
alt.lbbg$wind_tail_10_mean_abs <- abs(alt.lbbg$wind_tail_10_mean)
alt.lbbg$wind_side_10_mean_abs <- abs(alt.lbbg$wind_side_10_mean)
levels(alt.lbbg$wind.type)
mod.alt.lbbg <- glmer(alt_median ~ 
                       # wind_side_10_mean_abs +
                       wind_tail_10_mean_abs * wind.type +
                       # dist_straight +
                       # cloud_tot_mean +
                       # temp_k_mean +
                       # ecmwf_pressure_sea_lev_mean +
                       (1|ring_number),
                     family = gaussian,
                     data = alt.lbbg)

mod.alt.lbbg.std <-standardize(mod.alt.lbbg, standardize.y = FALSE)

mod.alt.lbbg.std <- mod.alt.lbbg

summary(mod.alt.lbbg.std)

r.squaredGLMM(mod.alt.lbbg.std)

mod.alt.lbbg.std.ci.Wald <- confint(mod.alt.lbbg.std, method="Wald")
# warnings()
plot(mod.alt.lbbg.std.ci.Wald)


library(lattice)
ci_dat <-mod.alt.lbbg.std.ci.Wald
ci_dat <- cbind(ci_dat, mean=rowMeans(ci_dat))
ci_df <- data.frame(coef=row.names(ci_dat), ci_dat)
names(ci_df)[2:3] <- c('lwr', 'upr')


lattice::dotplot(coef ~ mean, ci_df,
                 # xlim = c(-2,2),
                 #                  cexl.lab = 1.5, cex.axis = 1.5,
                 xlab = list("coef",cex=1.3),
                 panel = function(x, y) {
                   panel.segments(ci_df$lwr, y, ci_df$upr, y, lwd =2)
                   panel.xyplot(x, y, pch=18, cex = 1.2, col = "black")
                   panel.abline(v=0, lty=2)
                 },scales=list(y=list(cex=1.2), x = list(cex = 1.2))
)


# murre
alt.murre <- flights.all.alt[flights.all.va$species == "murre",]
str(alt.murre)
alt.murre$wind_tail_10_mean_abs <- abs(alt.murre$wind_tail_10_mean)
alt.murre$wind_side_10_mean_abs <- abs(alt.murre$wind_side_10_mean)
levels(alt.murre$wind.type)
mod.alt.murre <- glmer(alt_median ~ 
                        # wind_side_10_mean_abs +
                        wind_tail_10_mean_abs * wind.type +
                        # dist_straight +
                        # cloud_tot_mean +
                        # temp_k_mean +
                        # ecmwf_pressure_sea_lev_mean +
                        (1|ring_number),
                      family = gaussian,
                      data = alt.murre)

mod.alt.murre.std <-standardize(mod.alt.murre, standardize.y = FALSE)

mod.alt.murre.std <- mod.alt.murre

summary(mod.alt.murre.std)

r.squaredGLMM(mod.alt.murre.std)

mod.alt.murre.std.ci.Wald <- confint(mod.alt.murre.std, method="Wald")
# warnings()
plot(mod.alt.murre.std.ci.Wald)


library(lattice)
ci_dat <-mod.alt.murre.std.ci.Wald
ci_dat <- cbind(ci_dat, mean=rowMeans(ci_dat))
ci_df <- data.frame(coef=row.names(ci_dat), ci_dat)
names(ci_df)[2:3] <- c('lwr', 'upr')


lattice::dotplot(coef ~ mean, ci_df,
                 # xlim = c(-2,2),
                 #                  cexl.lab = 1.5, cex.axis = 1.5,
                 xlab = list("coef",cex=1.3),
                 panel = function(x, y) {
                   panel.segments(ci_df$lwr, y, ci_df$upr, y, lwd =2)
                   panel.xyplot(x, y, pch=18, cex = 1.2, col = "black")
                   panel.abline(v=0, lty=2)
                 },scales=list(y=list(cex=1.2), x = list(cex = 1.2))
)




# Power curves ------
flight.pcurve <- read.csv("power_curve.csv", header = TRUE)

str(flight.pcurve)

scaled_power <- flight.pcurve$checm_power
scaled_power[flight.pcurve$species == "murre"] <- scaled_power[flight.pcurve$species == "murre"]/77.02
scaled_power[flight.pcurve$species == "lbbg"] <- scaled_power[flight.pcurve$species == "lbbg"]/26.2

flight.pcurve$scaled_power <- scaled_power


resa = 72*4
png("pcurve_ggplot.png", res = resa, width = 8*resa, height = 6*resa)
# ?png
ggplot(flight.pcurve, aes(airspeed, scaled_power, color = factor(species) 
                            # shape = factor(year), 
                            ))+
  # geom_point(alpha = 60/100, colour="white", size = 2) +
  # geom_point(alpha = 70/100, size = 1.5)+
  scale_colour_manual(values=c("#3182bd", "#c51b8a"))+
  stat_smooth(method = "loess", size = 1.5)+
  # geom_line(size = 2)+
  scale_x_continuous(limits = c(8, 27))+
  # scale_y_continuous(limits = c(-25, 100)) +
  theme(legend.position="none") +
  theme(axis.text=element_text(size=18),
        axis.title=element_text(size=20,face="bold")) +
  geom_vline(xintercept = 18.4, color = "#3182bd", linetype = "longdash", size = 1) +
  geom_vline(xintercept = 25.6, color = "#c51b8a", linetype = "longdash", size = 1) +
  labs(x = expression(paste("Air speed (ms",""^{-1}, ")")), y = "Power (relative to minimum)")
# ?stat_smooth
dev.off()





# Histograms of head-tail wind for two species ------

resa = 72*4
png("wind_head_tail_ggplot.png", res = resa, width = 8*resa, height = 6*resa)

ggplot(flights.all, aes(wind_tail_10_mean, fill = species)) +
  geom_density(alpha = 0.4) +
  scale_fill_manual(values=c("#3182bd", "#c51b8a"))+
  scale_colour_manual(values=c("#3182bd", "#c51b8a"))+
  scale_x_continuous(limits = c(-10, 10))+
  # scale_y_continuous(limits = c(-25, 100)) +
  theme(legend.position="none") +
  theme(axis.text=element_text(size=18),
        axis.title=element_text(size=20,face="bold")) +
labs(x = expression(paste("Wind speed (ms",""^{-1}, ")")))
dev.off()

