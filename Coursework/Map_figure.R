## MAP FIGURE ##

# code provided by Chat-GPT for the figure

install.packages(c("terra", "sf", "ggplot2", "rnaturalearth", "rnaturalearthdata"))
library(terra)
library(sf)
library(ggplot2)
library(rnaturalearth)
library(rnaturalearthdata)

data2 <- read.csv("~/Documents/WorkingD/Meta-Analysis/Coursework/Meta_data3.csv", header = T)

data2$longitude <- as.numeric(data2$longitude)
data2$latitude <- as.numeric(data2$latitude)

# Convert to terra SpatVector
sites_vect <- vect(data2, geom = c("longitude", "latitude"), crs = "EPSG:4326")
sites_df <- as.data.frame(sites_vect, xy = TRUE)

world_sf <- ne_countries(scale = "medium", returnclass = "sf")
world_vect <- vect(world_sf)  # convert to terra SpatVector

# Create Map showing spatial bias of studies
quartz()
par(mfrow=c(1,1))
plot(world_vect, col = "wheat2", border = "grey40", xlab = 'Longitude', ylab="Latitude")
points(sites_vect, col = "red", pch = 20, cex = 1.2)

