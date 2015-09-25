# DB package
library("RODBC")

# Plotting histogram - flight speeds for
# LBBG + murres + wind??


# Code for transparent colours ----
# Code from https://github.com/mylesmharrison/colorRampPaletteAlpha/blob/master/colorRampPaletteAlpha.R
# Hight-lighted by blog post: http://www.everydayanalytics.ca/2014/03/colorRampPalette-alpha-in-R.html
addalpha <- function(colors, alpha=1.0) {
  r <- col2rgb(colors, alpha=T)
  # Apply alpha
  r[4,] <- alpha*255
  r <- r/255.0
  return(rgb(r[1,], r[2,], r[3,], r[4,]))
}

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


murres <- sqlQuery(gps.db,
                          query = "SELECT guillemots_gps_flights.*
FROM guillemots_gps_flights;
                   ",
                          as.is = TRUE)



murre_f <- murres$type == "final" &
  murres$duration > 600 &
  murres$duration < 6000
summary(murre_f)


# speeds ----
murre_vg <- murres$speed_median[murre_f]
lbbg_vg <- lbbg$speed_inst_med
wind_lbbg <- lbbg$windspeed

mean(murre_vg, na.rm = TRUE)
mean(lbbg_vg, na.rm = TRUE)
mean(wind_lbbg, na.rm = TRUE)



summary(is.na(murre_vg))

# Combined histogram ----
lbbg_d <- density(lbbg_vg)
murre_d <- density(murre_vg, na.rm = TRUE)
wind_d <- density(wind_lbbg)



# ?png
png("speeds_murre_lbbg_wind.png", width = 1200, height = 600)
plot(lbbg_d, type = "n", xlim = c(-1,30),
     ylim = c(0,0.25),
     yaxt = "n", ylab = "",
     xlab = "Speed (ms-1)",
     main = "",
     cex.lab = 2.0,
     cex.axis = 1.8)
# ?plot
polygon(d, col = "light blue", lwd = 2)
polygon(murre_d, col = addalpha("pink", alpha = 0.6), lwd = 2)
polygon(wind_d, col = addalpha("grey", alpha = 0.6), lwd = 2)
dev.off()

mean()




# altitude ----
murre_alt_uva <- murres$alt_median[murre_f & murres$device_type == "uva"]
murre_alt_igu <- murres$alt_median[murre_f & murres$device_type == "igu"]
murre_alt <- murres$alt_median[murre_f]

lbbg_alt <- lbbg$alt_med
# wind_lbbg <- lbbg$windspeed

mean(murre_alt, na.rm = TRUE)
mean(lbbg_alt, na.rm = TRUE)

mean(murre_alt_uva, na.rm = TRUE)
mean(murre_alt_igu, na.rm = TRUE)


# mean(wind_lbbg, na.rm = TRUE)



# summary(is.na(murre_vg))

# Combined histogram ----
lbbg_d_alt <- density(lbbg_alt)
murre_d_alt <- density(murre_alt, na.rm = TRUE)
murre_d_alt_igu <- density(murre_alt_igu, na.rm = TRUE)
murre_d_alt_uva <- density(murre_alt_uva, na.rm = TRUE)

# wind_d <- density(wind_lbbg)
hist(murre_alt, na.rm = TRUE, breaks = 40)
# ?png
png("speeds_murre_lbbg_alt.png", width = 1200, height = 600)
plot(lbbg_d, type = "n", xlim = c(-50,100),
     ylim = c(0,0.1),
     yaxt = "n", ylab = "",
     xlab = "Altitude (m)",
     main = "",
     cex.lab = 2.0,
     cex.axis = 1.8)
# ?plot
polygon(lbbg_d_alt, col = "light blue", lwd = 2)
polygon(murre_d_alt_uva, col = addalpha("pink", alpha = 0.6), lwd = 2)
polygon(murre_d_alt_igu, col = addalpha("pink", alpha = 0.6), lwd = 2)

# polygon(wind_d, col = addalpha("grey", alpha = 0.6), lwd = 2)
dev.off()



x <- murres[murres$alt_median > 50 & !is.na(murres$alt_median),]



# Violin plots -----


boxplot(lbbg_alt, na.rm = TRUE)

install.packages("vioplot")
library("vioplot")
vioplot(lbbg_alt, murre_alt[!is.na(murre_alt)], ylim = c(-100,150), h = 3)
?vioplot

hist(lbbg_alt, breaks = 40)
