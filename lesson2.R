## LESSON 2 - Manipulating - Geospatial Data in R
## introduction-to-gis-r
### Creating sf Objects------

# install.packages("sf")
library(sf)

# chama um shapefile usando a funçao st_read(path)
my_sf <- st_read("viz_india-master/india_states_2014/india_states.shp")

### Inspecting Objetcts----

# Before working with this sf object, let's briefly compare the sf and sp packages.
# First, we'll convert the sf object to a SpatialPolygonsDataFrame, an S4 class defined by the sp package. We can do this with sf::as(). 

my_spdf <- as(my_sf, "Spatial")
class(my_spdf)
