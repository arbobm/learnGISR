## LESSON 5 - Performing Spatial Subsetting in R
## https://atlan.com/courses/introduction-to-gis-r/lesson5-spatial-subsetting/

## importante https://r-spatial.github.io/sf/articles/sf1.html

## pacotes usados
# install.packages("tigris")
# install.packages("sf")
# install.packages("tidycensus")
install.packages("tidyverse")
install.packages("rvest")

api_key <- "YOUR_API_KEY"
census_api_key(api_key)                            



## What Is Spatial Subsetting?

### “Spatial subsetting is the process of selecting features of a spatial object based on
### whether or not they in some way relate in space to another object.” 

## To take an example using our previous data set of Indian states, we might wish to 
## filter for only states that share a border with Delhi NCR. Or, rather than filtering by 
## attributes like states or districts, we may only care about states or districts within 
## a certain distance from a particular point. Spatial subsetting operations allow us to 
## perform these kinds of manipulations. 

### Topological Relations----

# The two examples given above describe different topological relations. The former is 
# looking for a common border, or perhaps areas that "touch", whereas the latter is looking 
# for areas "within" another area. 

# As implemented in the sf package, you'll find these operations in functions like 
# st_intersects(),
# st_disjoint(), 
# st_within(),
# st_contains(),
# st_touches(),
# st_crosses() and more.
# The documentation for any of these functions includes the complete list.

# These functions require a pair of sf geometry sets — a target object and a selecting object. 
# Before diving into the specific syntax, let’s first get a sense of how these relations 
# are defined.

# The simplest is st_intersects() and its inverse st_disjoint(). 
# Giving two sf objects to st_intersects() will return all observations that intersect with 
# each other in any way. 
# Conversely, st_disjoint() returns observations with no intersection. 

# Another pair of operators is st_within() and st_contains(). 
# Both of these operations return only observations that lie entirely within one object or 
# another. The designation of "x" and "y" arguments determines whether st_within() or 
# st_contains() is actually the operation you need. 
# st_within() returns observations in "x" that fall entirely within "y". 
# st_contains() returns observations in "x" that entirely contain "y". 

### Preparing Data----

# We'll use the tidycensus and tigris packages to download median household income data for 
# the Philadelphia metro area at the census tract level. 

library(tigris)
library(sf)
library(tidycensus)
library(tidyverse)
library(rvest)
options(tigris_class = "sf")
options(tigris_use_cache = TRUE)

api_key <- "7123bf20309ad1afa683e44d6d1452a6a564177e"
census_api_key(api_key)                            

# create df of states and counties
counties <- tribble(
  ~state, ~county,
  "PA", "Philadelphia",
  "PA", "Montgomery", 
  "PA", "Bucks",
  "PA", "Delaware",
  "NJ", "Burlington",
  "NJ", "Camden",
  "NJ", "Gloucester"
)

# query tidycensus and combine data into one sf object
raw_tracts <- map2(counties$state, counties$county, function(x, y) {
  get_acs(geography = "tract", state = x, county = y,
          variables = c(hhincome = "B19013_001"), 
          geometry = TRUE)
}) %>% 
  do.call(rbind, .)

glimpse(raw_tracts)

## We now have 1,186 census tracts covering the Philadelphia metropolitan area. 
## This is a larger area than we want to cover so we'll spatially subset this data set to
## a smaller area based on distance from a central point of interest, in this case
## Philadelphia’s City Hall. 

## Before we can do this, however, it's important to pay attention to the coordinate 
## reference system (CRS) of our geospatial data. The commands below show that the data 
## has a geographic CRS with EPSG code 4269.

st_crs(raw_tracts)
st_is_longlat(raw_tracts)                  

# In order to use spatial subsetting operations, we need to reproject our data from a
# geographic CRS to a projected CRS. In this case, we’ve chosen to use EPSG code 2272.                     

proj_crs <- 2272
proj_tracts <- raw_tracts %>% 
  st_transform(crs = proj_crs)
st_crs(proj_tracts)
st_is_longlat(proj_tracts)

## Now that we have projected census tracts, we'll define a circle and use it as the 
## second geometry feature set by which we'll subset the census tracts. 

# choose a central long-lat point and radius to define circle
city_hall_lng <- -75.1657936
city_hall_lat <- 39.952383
geo_crs <- 4326
buffer <- 55000 # ft (same units as crs)

circle <- st_sfc(st_point(c(city_hall_lng, city_hall_lat)), 
                 crs = geo_crs) %>% 
  st_transform(crs = proj_crs) %>% 
  st_buffer(dist = buffer)

st_crs(circle)

plot(proj_tracts['variable'])

### Spatial Subsetting Syntax----

## In R, there are often multiple ways to achieve the same result. For subsetting, we have 
## a base R method using the square bracket [ and a tidyverse method using filter(). 
## Spatial subsetting is no exception — both options are available within the sf package.                                                                                                                                   The syntax is remarkably simple with the square bracket method. It's very similar to bracket subsetting of a dataframe. But inside the square bracket, where a logical expression would filter rows, you just need to place the selecting simple feature geometry (i.e. a spatial dataframe, sfc_POLYGON, etc).

## In the example below, we subset the original 1,186 census tracts by those that intersect 
## the circle we defined. The result is a new sf spatial dataframe with 617 observations.

philly <- proj_tracts[circle,]
glimpse(philly)

plot(philly['variable'])                        

## By default, st_intersects() is the unspoken topological operator when using the square 
## bracket for spatial subsetting. Setting the "op" argument allows us to choose any 
## topological relation instead of the default st_intersects. In the example below, we’ve 
## chosen st_disjoint(). 

philly_dj <- proj_tracts[circle, , op = st_disjoint]
nrow(philly_dj) 
glimpse(philly_dj) 

## A second method of spatial subsetting involves creating an intermediary object of the 
## class "sparse geometry binary predicate" (sgbp), which is essentially a list of matching 
## indices we can use to subset the target object. Under this method, rather than setting an
## "op" argument, we use a different topological operator beginning with st_*.

## Moreover, we have the option of returning a sparse or a dense matrix, which slightly 
## affects the syntax as shown below. This method fits more easily into a tidy workflow, 
## as evidenced by the use of dplyr. 

# sgbp, sparse matrix
philly_sparse <- proj_tracts %>% 
  filter(lengths(st_intersects(x = ., y = circle)) > 0)

# sgbp, dense matrix
philly_dense <- proj_tracts %>%
  filter(st_intersects(x = ., y = circle, sparse = FALSE))

## Regardless of which method you choose, all three methods should return the same number
## of observations in a spatial dataframe of the same CRS. 

## To give one example of this tidy workflow, note below how we can start with our original 
## spatial dataframe, perform a spatial subset (in this case st_within), and directly pipe
## the result into ggplot2. As we'd expect, the result is a much smaller and more circular 
## shape, fitting just inside the boundaries of our circle. 

city_hall <- st_sfc(
  st_point(c(city_hall_lng, city_hall_lat)), 
  crs = geo_crs) %>% 
  st_transform(crs = proj_crs) 

proj_tracts %>%
  filter(st_within(x = ., y = circle, sparse = FALSE)) %>% 
  ggplot() +
  geom_sf() +
  geom_sf(data = circle, color = "red", fill = NA) +
  geom_sf(data = city_hall, color = "blue", size = 2) +
  labs(title = "Census Tracts Within 55,000 ft of City Hall")

### Explore Spatial Subsetting via Shiny----

## While seeing the syntax and a few diagrams can be useful, it's even better to repeatedly 
## practice with different operations and quickly see the results. This Shiny app will let 
## you quickly explore spatial subsetting through different topological relations. 




