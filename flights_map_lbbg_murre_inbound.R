

# Script to map both lbbg and murre flights

# Get data -----
# Get flight info - murre


# Get flight info - lbbg


# Get flight GPS locations - lbbg


# Get flight GPS locations - murre



# Function to plot a base map ----
# Plot a map with coastlines only - no 
library(maps)

# Coastline data

# Colours - perhaps land in pastil green, sea light-grey?

# Include scale

# Lat/ long axes labels











# Location maps -----
# World maps
map('world', project = "mollweide")
map('world', project = "globular")
map('world', project = "gilbert")
points(mapproject(list(y=59, x=20)), col= "red", cex=2, pch = 8)
