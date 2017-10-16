# Combine and tidy-up razorbill GPS data ----


# Load in 2010 data ----
gps_8114307 <- read.csv("D:/Dropbox/Razorbills/maps/gps_files/8114307_2010.csv")

# Fix date_time
gps_8114307$date_time <- as.POSIXct(as.character(gps_8114307$time), format = "%Y-%m-%dT%H:%M:%OSZ", tz = "UTC")
gps_8114307$year <- 2010
gps_8114307$bird_id <- "8114307"


# Load in 2011 data ----
gps_8114325_2011  <- read.csv("D:/Dropbox/Razorbills/maps/gps_files/8114325_2011.csv")
gps_8114326_2011  <- read.csv("D:/Dropbox/Razorbills/maps/gps_files/8114326_2011.csv")
gps_8114328_2011  <- read.csv("D:/Dropbox/Razorbills/maps/gps_files/8114328_2011.csv")

gps_8114325_2011$bird_id <- "8114325"
gps_8114326_2011$bird_id <- "8114326"
gps_8114328_2011$bird_id <- "8114328"

gps_8114325_2011$year <- 2011
gps_8114326_2011$year <- 2011
gps_8114328_2011$year <- 2011

gps_2011 <- rbind.data.frame(gps_8114325_2011, gps_8114326_2011, gps_8114328_2011)

gps_2011$date_time <- as.POSIXct(paste(as.character(gps_2011$Date),
                                       as.character(gps_2011$Time), sep = " "),
                                 tz = "UTC")


# Load in 2015 data ----
# Need to reprocess with function thing...
source("parse_igotu2gpx_txt.R")

gps_8114401_2015 <- parse.file(file = "D:/Dropbox/Razorbills/maps/gps_files/8114401_2015.txt")

gps_8114404_2015 <- parse.file(file = "D:/Dropbox/Razorbills/maps/gps_files/8114404_2015.txt")


gps_8114401_2015$bird_id <- "8114401"
gps_8114404_2015$bird_id <- "8114404"

gps_8114401_2015$year <- 2015
gps_8114404_2015$year <- 2015

# Remove pre/ post-deployment data
gps_8114401_2015 <- gps_8114401_2015[-c(1:282),]
gps_8114404_2015 <- gps_8114404_2015[-c(1:6,418:472),]

# Combine 2015 data
gps_2015 <- rbind.data.frame(gps_8114401_2015, gps_8114404_2015)

# Remove non location lines
gps_2015 <- gps_2015[!is.na(gps_2015$lat + gps_2015$long), ]
gps_2015 <- gps_2015[(gps_2015$lat != 0), ]



# Merge all data together -----
name.vec <- c("lat","long","date_time",
              "year","bird_id")
gps_all <- rbind.data.frame(setNames(gps_8114307[,c("lat","lon","date_time",
                                                    "year","bird_id")],name.vec),
                            setNames(gps_2011[,c("Latitude","Longitude","date_time",
                                        "year","bird_id")], name.vec),
                            setNames(gps_2015[,c("lat","long","date_time",
                                        "year","bird_id")], name.vec),
                            make.row.names = FALSE)

save(gps_all, file = "razo_gps_all.RData")
