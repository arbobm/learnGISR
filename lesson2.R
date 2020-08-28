## LESSON 2 - Manipulating - Geospatial Data in R
## introduction-to-gis-r
### Creating sf Objects------

### instala pacotes necessários----
# install.packages("sf")
# install.packages("backports") # tidyverse pediu esse pra poder instalar, não sei porque
# install.packages("dplyr")
# install.packages("lwgeom")
# install.packages("rmapshaper")
# install.packages("pryr") # mostra tamanho dos objetos vetoriais

library(sf)

# chama um shapefile usando a funçao st_read(path)
my_sf <- st_read("viz_india-master/india_states_2014/india_states.shp")

### Inspecting Objetcts----

# Before working with this sf object, let's briefly compare the sf and sp packages.
# First, we'll convert the sf object to a SpatialPolygonsDataFrame, an S4 class defined by the sp package. We can do this with sf::as(). 

my_spdf <- as(my_sf, "Spatial")
class(my_spdf) # SpatialPolygonsDataFrame
class(my_sf)   # sf object

# inspect the struture of a SpatialPolygonsDataFrame
str(my_spdf, max.level = 2)

# We notice it has 5 "slots" (each prefaced by the @ symbol). 
# The first slot should look familiar. The data slot holds a dataframe with 36 observations of 8 variables. 
# We can extract any of these slots using the @ symbol like we'd normally do with the $ symbol. 

library(tibble)
glimpse(my_spdf@data)

# The following slots hold polygons, plotOrder, bbox and proj4string. 
# We'll return to these in the context of sf objects.
# For now, just note that the data and aspects of the object’s geometry are held in separate slots.
# This is not compatible with the tidyverse-style workflow to which many of us have grown accustomed. 

# While we could continue working with this format, let’s convert it back to an sf object with the st_as_sf() function and inspect the difference. 

ind_sf <- st_as_sf(my_spdf)
class(ind_sf)

# The former SpatialPolygonsDataFrame, a class defined by the sp package, now has two simultaneous classes: sf and data.frame. Printing the first few observations tells us a lot about the object. 

View(head(ind_sf, 3))

# It has 36 features (depending on how many we print) and 8 fields (our attributes).
# The geometry type is a multipolygon because the geometries represent the shapes of various areas. Other common geometry types include points, lines, and their “multi-” counterparts.
# bbox gives the object’s bounding box dimensions.
# epsg and proj4string describe the coordinate reference system (CRS). Note that this is a geographic CRS (measured in longitude and latitude) as opposed to a projected CRS.

# We could directly access information about the object’s spatial features with functions like
st_geometry_type(ind_sf)
st_dimension(ind_sf)
st_bbox(ind_sf) # extent
st_crs(ind_sf) # Coordinate Reference System

# functions that we'd use to explore a dataframe also work on sf objects
glimpse(ind_sf) 
View(ind_sf)

### Manipulating sf Objects----

#Because spatial dataframes in the sf package are dataframes, 
# we can manipulate them using our normal data manipulation tools, 
# such as dplyr. Here we'll just select and rename the columns we want. 

uts <- c("Delhi", "Andaman & Nicobar Islands", "Puducherry", 
         "Lakshadweep", "Dadra & Nagar Haveli", "Daman & Diu",
         "Chandigarh")
library(dplyr)

ind_sf <- ind_sf %>% 
  select(name, abbr) %>% 
  mutate(
    type = ifelse(name %in% uts, "Union Territory", "State")
  ) %>% 
  rename(abb = abbr, state_ut = name)

# For those already familiar with the tidyverse, using normal dplyr verbs to manipulate sf objects is one of the great benefits of using the sf package. If we were working with the slots of the earlier SpatialPolygonsDataFrame, this wouldn't be possible. Moreover, note that these manipulations haven't affected the class of our object in any way. 

### Preparing Attribute Data----

# see prepare_data.R script in github for details of creating attributes_df
attributes_df <- readRDS("./viz_india-master/attributes.rds")
colnames(attributes_df)

ind_sf <- ind_sf %>% 
  left_join(attributes_df, by = "state_ut") %>%
  mutate(
    per_capita_gdp_inr = nominal_gdp_inr / pop_2011,
    per_capita_gdp_usd = nominal_gdp_usd / pop_2011
  )


### Calculating Area----

colnames(ind_sf)

# we could calculate the area of each observation in our spatial dataframe using the st_area() function.
#It's simple enough to do this, but we need to be careful with the units. In this case, we need to convert from square meters to square kilometers. 

library(units) # calculo de área
library(lwgeom)

# mutate area
ind_sf <- ind_sf %>% 
  mutate(my_area = st_area(.))

# convert units
units(ind_sf$my_area) <- with(ud_units, km^2)

#mutate gdp density
ind_sf <- ind_sf %>% 
  mutate(gdp_density_usd_km2 = nominal_gdp_usd / my_area)

colnames(ind_sf)

# In the output below, note the difference between the simple numeric class of area_km2 and the class of "units" for the area calculation resulting from st_area(). 

class(ind_sf$area_km2)
class(ind_sf$my_area)
glimpse(ind_sf)

### Simplifying Geometry----

# Before ploting sf objects, we should simplify
# the polygons in the spatial dataframe

# Simplification can vastly reduce memory requirements while sacrificing very little in terms of visual output. Fortunately, there's an easy process to reduce the number of vertices in a polygon while retaining the same visible shape. 

# One option is sf::st_simplify(), but here we'll use the ms_simplify() function from the rmapshaper package.
# Below we keep only 1% of the object’s vertices while maintaining the same number of shapes. 

## NOTE:
# Another useful function is sf::st_geometry(). When passing it an sf object, it will return just the geometry. This allows us to create a quick plot of only the geometry to check if everything looks right.
# We also stripped the units class for the area we calculated because it created a problem for ms_simplify(). We can always add it after simplification.

z <- st_geometry(ind_sf)
class(z)
class(ind_sf)

# strip units class
ind_sf <- ind_sf %>% 
  mutate (
    my_area = as.vector(my_area),
    gdp_density_usd_km2 = as.vector(gdp_density_usd_km2)
  )
glimpse(ind_sf)

original_geometry <- st_geometry(ind_sf)
class(original_geometry)

library(rmapshaper)
simp_sf <- ms_simplify(ind_sf, keep = 0.01, keep_shapes = TRUE)
class(simp_sf)
simple_geometry <- st_geometry(simp_sf)
class(simple_geometry)

par(mfrow = c(1,2))
plot(original_geometry, main = "Original Geometry")
plot(simple_geometry, main = "Simplified Geometry")


#The original map is above on the left, and the simplified version is on the right. The simplified version looks no different despite having only 1% of the vertices. In fact, it looks even better because the border lines are cleaner.
#Moreover, simplification reduced the geometry size from 9.56 MB to just 150 KB. 

library(pryr) 

object_size(original_geometry)
object_size(simple_geometry)

# Finally, let’s save the simplified spatial dataframe for the next lesson. 
saveRDS(simp_sf, "simp_sf.rds")
