# Import libraries
library(tidyverse)
library(tidygraph)
library(cshapes)
library(leaflet)
library(sf)
library(units)
library(lwgeom)
library(rgeos)
library(rgdal)
library(ggplot2)
library(prioritizr)
library(tmap)
library(ggraph)
library(ggdag)
library(igraph)
library(stringr)

#(1)
# Save countries borders as of date 2019-12-31
cmap.2019 <- cshp(date=as.Date("2019-12-31"))

# Check if geometries are valid. If not fix them
st_is_valid(cmap.2019[0], reason=TRUE)
validcmap.2019 <- st_make_valid(cmap.2019)

# Obtain Greece information
greece_df <- filter(validcmap.2019, country_name=='Greece')

# Get the distances from Greece for all countries and attach them to the df
distance_from_greece <- st_distance(validcmap.2019, greece_df)
final_data <- cbind(validcmap.2019, distance_from_greece)

# Drop the [m] units that label the numbers and transfor them into km.
final_data$distance_from_greece <- drop_units(final_data$distance_from_greece)
final_data$distance_from_greece <- final_data$distance_from_greece / 1000

# Only keep km lower than 2000
final_data <- final_data %>% filter(final_data$distance_from_greece <= 2000)

# Plot a simple map for distances
plot(final_data[12])

#Make tmap interactive
tmap_mode("view")

# Plot a map for distances using tm_shape
tm_shape(final_data)+tm_polygons()

# Plot a map for distances using leaflet
leaflet() %>% 
  addTiles() %>% 
  addPolygons(data = final_data, color = "red", weight = 1,
              fillOpacity = 0.3, fillColor = "blue")


#(2)
# Turn off Spherical geometry (s2)
sf_use_s2(FALSE)

# Save countries borders as of date 2019-1-1
cmap.2019 <- cshp(date=as.Date("2019-1-1"))
cmap.2019 <- st_make_valid(cmap.2019)

# Check object class
class(cmap.2019)

# Make WGS84 default CRS
cmap.2019_wgs84 = st_transform(cmap.2019, "EPSG:4326")

# Use leaflet to create an interactive map with capital names
leaflet() %>% 
  addTiles() %>% 
  addPolygons(data = cmap.2019_wgs84, color = "red", weight = 1,
              fillOpacity = 0.3, fillColor = "blue") %>%
  addMarkers(data = cmap.2019_wgs84, 
             lng = cmap.2019_wgs84$caplong, 
             lat = cmap.2019_wgs84$caplat, 
             label = cmap.2019_wgs84$capname)

# Use tm_shape to create an interactive map with capital names
tm_shape(validcmap.2019)+tm_polygons() +tm_text('capname')+tm_dots() 


#(3)
# Turn on Spherical geometry (s2)
sf_use_s2(TRUE)

# Obtain capital coordinates for every country and project them
# accordingly to meet our standards. After put the coords into
# the world df.
capital_coords <- data.frame(lon = cmap.2019_wgs84$caplong,
                             lat = cmap.2019_wgs84$caplat)
coordinates(capital_coords) <- c("lon", "lat")
proj4string(capital_coords) <- CRS("+init=epsg:4326")
capital_coords_sf <- st_as_sf(capital_coords)
names(capital_coords_sf)[1] <- "capital_coord"
cmap.2019_wgs84 <- cbind(cmap.2019_wgs84, capital_coords_sf)

# Create a buffer of 100 km around each capital and add it to the
# main world df.
buffer_100km <- st_buffer(cmap.2019_wgs84$capital_coord, dist = 100000)
cmap.2019_wgs84['capital_buffer'] <- buffer_100km

# Calculate the intersections that arise with the buffer and convert
# numerical into logical values.
intersections <- st_intersects(buffer_100km, validcmap.2019, remove_self = TRUE)
true_false_intersections = lengths(intersections) > 0
intersecting_countries <- validcmap.2019[true_false_intersections, ]

# Plot a basic map with countries in buffer
plot(intersecting_countries[1])

# Use tm_shape to create an interactive map with countries in buffer
tm_shape(intersecting_countries)+tm_polygons()+tm_text('country_name')

# Use leaflet to create an interactive map with countries in buffer
leaflet() %>% 
  addTiles() %>% 
  addPolygons(data = intersecting_countries, color = "red", weight = 1,
              fillOpacity = 0.3, fillColor = "blue") %>%
  addMarkers(data = intersecting_countries, 
             lng = intersecting_countries$caplong, 
             lat = intersecting_countries$caplat, 
             label = intersecting_countries$capname)


#(4)
cmap.2019_wgs84 <- st_make_valid(cmap.2019_wgs84)

# Calculate centroid for each country
country_centroid <- st_centroid(cmap.2019_wgs84$geometry)

# Calculate distance from centroid to capital for every country
distance_km <- st_distance(cmap.2019_wgs84$capital_coord, country_centroid, by_element = TRUE)

# Put distances into the world df, drop meters and divibe by
# 1000 in order to have km as units.
cmap.2019_wgs84 <- cbind(cmap.2019_wgs84, distance_km)
cmap.2019_wgs84$distance_km <- drop_units(cmap.2019_wgs84$distance_km)
cmap.2019_wgs84$distance_km <- cmap.2019_wgs84$distance_km/1000

# Filter the df accordingly to get what we want
filtered_100 <- filter(cmap.2019_wgs84, cmap.2019_wgs84$distance_km < 100)
filtered_100_300 <- filter(cmap.2019_wgs84, cmap.2019_wgs84$distance_km >= 100, cmap.2019_wgs84$distance_km <= 300)
filtered_300 <- filter(cmap.2019_wgs84, cmap.2019_wgs84$distance_km > 300)

# Plot the countries based on the distance_km variable
tm_shape(filtered_100)+tm_polygons()+tm_text('country_name')
tm_shape(filtered_100_300)+tm_polygons()+tm_text('country_name')
tm_shape(filtered_300)+tm_polygons()+tm_text('country_name')

#(5)
# Calculate distance matrix with function
distance_matrix <- distmatrix(as.Date("2019-1-1"), "capdist")

# Rows and columns are named after capitals.
rownames(distance_matrix) <- cmap.2019_wgs84$capname
colnames(distance_matrix) <- cmap.2019_wgs84$capname

# Convert distance matrix into df.
distance_matrix <- as.data.frame(distance_matrix)

# Add a new column with the names of capitals.
distance_matrix["capitals"] <- cmap.2019$capname
distance_matrix
