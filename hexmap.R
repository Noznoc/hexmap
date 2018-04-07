# SET WORK DIRECTORY
setwd("C:/Users/Julia/Documents/Code/r-maps")

# INSTALL REQUIRED PACKAGES IF THEY ARE NOT INSTALLED
required <- c("GISTools", "sp", "raster", "rgeos", "maptools", "leaflet", "ggmap", "tidyverse", "classInt", "scales", "base", "mapview")
pkgs <- rownames(installed.packages())
need <- required[!required %in% pkgs]
if(length(need)) install.packages(need)

# RUN LIBRARIES
library(tidyverse)
library(rgdal)
library(sp)
library(raster)
library(rgeos)
library(leaflet)
library(GISTools)
library(ggmap)
library(maptools)
library(classInt)
library(scales)
library(base)

# ---- THE FOLLOWING CODE IS FOR CREATING + EXPORTING HEXAGON GRID(S) FOR STUDY AREA / SAMPLE AREAS ----

# READ + CREATE SAMPLE AREA FROM STUDY AREA
sa <- rgdal::readOGR("./data", "local_area_boundary")
sa <- unionSpatialPolygons(sa, sa@polygons)
crime <- read_csv("./data/crime_csv_all_years.csv", locale = locale(encoding="latin1"))

# MAKE GRID FUNCTION
make_grid <- function(x, cell_diameter, cell_area, clip = TRUE) {
  if (missing(cell_diameter)) {
    if (missing(cell_area)) {
      stop("Must provide cell_diameter or cell_area")
    } else {
      cell_diameter <- sqrt(2 * cell_area / sqrt(3))
    }
  }
  ext <- as(extent(x) + cell_diameter, "SpatialPolygons")
  projection(ext) <- projection(x)
  # generate array of hexagon centers
  g <- spsample(ext, type = "hexagonal", cellsize = cell_diameter, 
                offset = c(0.5,0.5)) #  the offset (position) of the regular grid; the default for spsample methods is a random location in the unit cell [0,1] x [0,1], leading to a different grid after each call; if this is set to c(0.5,0.5), the returned grid is not random (but, in Ripley's wording, "centric systematic"). For line objects, a single offset value is taken, where the value varies within the [0, 1] interval, and 0 is the beginning of each Line object, and 1 its end
  # convert center points to hexagons
  g <- HexPoints2SpatialPolygons(g, dx = cell_diameter)
  # clip to boundary of study area
  if (clip) {
    g <- gIntersection(g, x, byid = TRUE)
  } else {
    g <- g[x, ]
  }
  # clean up feature IDs
  row.names(g) <- as.character(1:length(g))
  return(g)
}

# MAKE GRID (note: cell_area is in meters, same as the projection)
hexgrid <- make_grid(sa, cell_area = 100000, clip = TRUE)
plot(sa, axes = FALSE)
plot(hexgrid, border = "purple", add = TRUE)
box()

# PREPARE SPATIAL POINTs DATA FRAME
crime_coords <- crime %>% 
  dplyr::select(X, Y)
crime_spdf <- SpatialPointsDataFrame(crime_coords, data = crime, proj4string=CRS("+proj=utm +zone=10 +datum=NAD83 +units=m +no_defs +ellps=GRS80 +towgs84=0,0,0"))
crime_transform <- spTransform(crime_spdf, CRS("+proj=utm +zone=10 +datum=NAD83 +units=m +no_defs +ellps=GRS80 +towgs84=0,0,0"))

# CONVERT HEX GRID TO SPATIAL POLYGONS DATA FRAME USING DUMMY DATA
dummy <- data.frame(id_grid = seq.int(length(hexgrid@polygons)))
hexgrid_temp <- SpatialPolygonsDataFrame(hexgrid, dummy)
hexgrid_temp@data <- hexgrid_temp@data %>% 
  mutate(id_grid = as.numeric(rownames(hexgrid_temp@data)))

# SPATIAL JOIN AND AGGREGATE ATTRIBUTE DATA FOR MAP VIS
crime_transform@data <- mutate(crime_transform@data, id_crime = as.numeric(rownames(crime_transform@data))) # create ids for each crime
intersect <- over(crime_transform, hexgrid_temp) # spatial join function
intersect <- mutate(intersect, id_crime = as.numeric(rownames(intersect)))
intersect <- left_join(crime_transform@data, intersect, by = c("id_crime" = "id_crime"))
crime_join <- left_join(crime_transform@data, intersect, by = c("id_crime" = "id_crime"))
crime_attributes <- intersect %>% group_by(id_grid) %>%
  count(TYPE) %>%
  slice(which.max(n)) %>% # add additional attributes to the grid
  arrange(id_grid)
hexgrid_temp@data <- left_join(hexgrid_temp@data, crime_attributes, by = c("id_grid" = "id_grid"))
hexgrid_attr <- hexgrid_temp@data$TYPE

# TRANSFORM PROJECTION FROM ALBERT CONFORMAL CONIC TO LONGLAT FOR MAP VIS
hexgrid_longlat <- spTransform(hexgrid_temp, CRS("+proj=longlat +datum=WGS84"))

# CREATE PALETTE FOR CATEGORICAL DATA (CRIME TYPE)
pal <- colorFactor("Paired", hexgrid_attr, na.color = "transparent")

# CREATE MAP
map <- leaflet() %>% 
  addProviderTiles(providers$OpenStreetMap.BlackAndWhite) %>%
  addPolygons(data = hexgrid_longlat, stroke = TRUE, color = "#333", weight = 1, smoothFactor = 0.5,
              opacity = 0.1, fillOpacity = 0.8, fillColor = ~pal(hexgrid_attr),
              highlight = highlightOptions(
                weight = 2,
                fillOpacity = 0,
                bringToFront = TRUE)) %>%
  addLegend("topright", pal = pal, values = hexgrid_attr, opacity = 1, title = "Top Crime", na.label = "No Data") %>%
  addScaleBar(position = "bottomleft", options = scaleBarOptions(imperial = FALSE))
map