# Script to parse data from movebank - then put this into Access DB


# Import data from text file ------
movebank <- read.csv("D:/Dropbox/Guillemots/move_bank_data/birds murres sweden-8369105148982838530.csv",
                     header = TRUE)


# Delete unrequired columns -----
movebank.new <- movebank[,-c(1,2,4,5,6,7,10)]

# Fix names ----
names(movebank.new) <- c("date_time", "device_info_serial",
                         "ring_number", "ecmwf_wind_10m_v",
                         "ecmwf_surf_roughness", "ecmwf_cloud_cov_low",
                         "ecmwf_sst", "ecmwf_wind_10m_gust",
                         "ecmwf_cloud_cov_tot", "ecmwf_charnock",
                         "ecmwf_boundary_lay_ht", "ecmwf_ppt_tot",
                         "ecmwf_temp_2m", "ecmwf_wind_10m_u",
                         "ecmwf_pressure_sea_lev")


# Fix data structure -------
str(movebank.new)

# fix date_time
movebank.new$date_time <- as.POSIXct(movebank.new$date_time, tz = "UTC") 

str(movebank.new)

# Output to DB -----

# DB package
library("RODBC")

# Connect to DB
gps.db <- odbcConnectAccess2007('D:/Dropbox/tracking_db/murre_db/murre_db.accdb')



#will be neccessary to edit table in Access after to define data-types and primary keys and provide descriptions for each variable.
sqlSave(gps.db, movebank.new,
        tablename = "guillemots_gps_points_movebank_ecmwf",
        append = FALSE, rownames = FALSE, colnames = FALSE,
        verbose = FALSE, safer = TRUE, addPK = FALSE, fast = TRUE,
        test = FALSE, nastring = NULL,
        varTypes =  c(date_time = "datetime")
)







# Visualise some of this data -----
hist(movebank.new$ecmwf_boundary_lay_ht, breaks = 100)
hist(movebank.new$ecmwf_boundary_lay_ht, breaks = 100, xlim = c(0,200))

# hist(movebank.new$ecmwf_wind_10m_v)
