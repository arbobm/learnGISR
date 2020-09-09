## LESSON 3 - Creating Static Maps in R
## https://atlan.com/courses/introduction-to-gis-r/lesson3-static-maps/
## introduction-to-gis-r

# This lesson introduces how to use some of the most well-known R packages to create static maps, such
#as tmap and ggplot2.
# We’ll also explore a few other packages like cartogram, geogrid and geofacet for some more unique 
#spatial visualizations. 

## pacotes necessários---- 
# install.packages("tmap") #demoradinho
# install.packages("ggplot2")
# install.packages("cartogram")
# install.packages("geogrid")
# install.packages("geofacet")
# install.packages("ggrepael")
# install.packages("stringr")

### Resources on Visualizing Geospatial Data----

# Before diving into different R packages for mapping, let's review a few excellent resources that will
#help you get started. 

# Recent advances in software have made many different types of geospatial data visualizations — such
#as choropleths, dot density maps and cartograms — easily available. However, the correct 
#visualization often begins with the type of data you have. 

# Nature of our data. Is the data numeric? If so, is it a raw count, such as population
# or is it standardized, such as population density?
# If the data isn't numeric, is it nominal (or categorical), such as linguistic or religion data,
# or ordinal, such as satisfaction rankings?

### Static Maps----

# This section looks at how to create static maps in base R, tmap, and ggplot2. 
## They aren't the only options. The cartography package is another interesting tool, particularly
## for certain kinds of maps, such as choropleths contained in proportional symbols. 
## See the package vignette and cheat sheet to get started.
## https://cran.r-project.org/web/packages/cartography/vignettes/cartography.html
## https://raw.githubusercontent.com/riatelab/cartography/master/img/cheat_sheet.png

### Base Plotting----

# the sf package provides a plot() method for visualizing geographic data.

library(sf)
simp_sf <- readRDS("simp_sf.rds")
plot(simp_sf) # produce a grid of faceted plots, one for each attribute.
plot(simp_sf['pop_2011'])

# it is quick and easy to plot base maps
# there are reasons why this default choropleth may not be an effective visualization.

### Thematic Maps (tmap) ----

# it's especially well-suited for choropleths, but it can produce a wide range of geospatial visualizations.
# ggplot2-style syntax
# it emphasizes sequentially adding layers to a plot
# pass a spatial dataframe to the tm_shape() function much like you'd pass a dataframe to the ggplot() function
# Moreover, because spatial dataframes in the sf package are also dataframes, you can filter out any particular features (like "Andaman & Nicobar Islands" below) and directly proceed with piping the object into a tm_shape() chain. 

# map India's GDP density, a measure of economic activity by area
# units of nominal GDP per square kilometer

library(tmap) 
library(dplyr)
colnames(simp_sf)
simp_sf %>% 
  filter(!state_ut %in% c("Andaman & Nicobar Islands", "Lakshadweep")) %>% 
  tm_shape() +
  tm_fill(col = "pop_2011", title = "No. People") +
  tm_borders(lwd = 0.5) +
  tm_text("abb", size = 0.5) +
  tm_style("gray") +
  tm_layout(
    main.title = "Population (2011)",
    main.title.position = c("center"),
    main.title.size = 1,
    legend.position = c("right", "bottom")
  ) +
  tm_credits("Data:\n2011 Census", position = c("left", "bottom"))

### Arranging tmap objects----

# tmap_arrange() function for lining up multiple tmap objects next to each other
# example: if we filter out the small union territories to get a fairer distribution,
# we can separately create tmap objects of population growth and density
# then we can arrange them next to each other for comparison

states_sf <- simp_sf %>% 
  filter(!type == "Union Territory")
View(states_sf)
unique(simp_sf$type)

growth <- tm_shape(states_sf) +
  tm_fill(col = "decadal_growth", title = "Percentage") +
  tm_borders(lwd = 0.5) +
  tm_layout(
    main.title = "Population Growth of States (2001-2011)",
    main.title.position = c("center"),
    main.title.size = 1,
    legend.position = c("right", "bottom")
  ) +
  tm_credits("Data:\n2001-2011 Census", position = c("left", "bottom"))

density <- tm_shape(states_sf) +
  tm_fill(col = "density_km2", title = "No. People / Sq Km",
          palette = "YlGnBu") +
  tm_borders(lwd = 0.5) +
  tm_layout(
    main.title = "Population Density of States (2011)",
    main.title.position = c("center"),
    main.title.size = 1,
    legend.position = c("right", "bottom")
  ) +
  tm_credits("Data:\n2011 Census", position = c("left", "bottom"))

tmap_arrange(growth, density)

### Inset Maps----

# create inset maps, those that include a small window providing
# the wider geographic context for the main map

# create a base or primary map
# sex ratio Northeast India
# trick from tmap_tricks(): reverse the color scale by placing a - in front of the pallet name
# this malkes sense because our concern should increase as sex ratio decreases

ne_sex <- simp_sf %>% 
  filter(region == "Northeastern") %>% 
  tm_shape() +
  tm_fill(col = "sex_ratio", title = "Sex Ratio", 
          palette = "-Reds") +
  tm_borders(lwd = 0.5) +
  tm_text('state_ut', size = 0.75) +
  tm_layout(
    main.title = "Sex Ratio in India's Northeast",
    main.title.position = c("center"),
    main.title.size = 1
  ) +
  tm_credits("Data Source: Wikipedia", position = c("left", "top"))

# create the smaller inset map to provide the wider geographic context
# For the small map, we wanted to highlight the Northeast region on the larger map of India. 

# 1. group the features by region
# using the same dplyr syntax, we can reduce the 36 features to 8 regions
# these 8 regional features have a geometry reflecting the "sum" of their individual sub-components
# use st_unify()

regional_sf <- simp_sf %>% 
  group_by(region) %>% 
  summarise(pop = sum(pop_2011))

inset <- regional_sf %>% 
  filter(!region == "Arabian Sea",
         !region == "Bay of Bengal") %>% 
  mutate (northeast = ifelse(region == "Northeastern", TRUE, FALSE)) %>% 
  tm_shape() +
  tm_fill(col = "northeast", palette = c("grey", "red")) + 
  tm_style("cobalt") +
  tm_legend(show = FALSE)

# combine base map and inset map
# use some trial and error to get the placement right

library(grid)
ne_sex
print(inset, vp = viewport(0.24, 0.18, width = 0.2, height = 0.4)) # como fazer com que a localização do inset fique certa e fixa independentemente do redimensionamento da janela?


### Faceted Maps----
# tmap also supports the creation of faceted maps, or small multiples. They can be useful for attributes 
#with a fairly small number of levels. For instance, if we have population data for a few years, we could 
#show a progression over time.

# The free.coords argument controls whether to show only the faceted map area or instead highlight the 
#facet’s place in the original map. 

# There is no inherent order to regions, but it's useful to impose one. Below we've ordered the facets in 
#a roughly counter-clockwise order starting from "Northern". To do this, it helps to first make region an 
#ordered factor. 

# create custom labels for log scale
library(stringr)
gdp_seq <- 10 ^ (seq(2.8, 4.0, by = 0.2))
gdp_vec <- scales::dollar(round(gdp_seq))

my_labels <- vector(mode = "character", length = 6)
for(i in seq_along(1:6)){
  my_labels[i] = stringr::str_c(gdp_vec[i], " to ", gdp_vec[i + 1])
}

simp_sf %>% 
  mutate(
    log_pc_usd = log10(per_capita_gdp_usd),
    region_fac = factor(region, levels = c("Northern", "Western", "Southern",
                                           "Central", "Eastern", "Northeastern",
                                           "Arabian Sea", "Bay of Bengal")) 
  ) %>%
  filter(!state_ut %in% c("Andaman & Nicobar Islands", 
                          "Lakshadweep")) %>% 
  tm_shape() +
  tm_borders(lwd = 0.5, col = "white") +
  tm_fill(col = 'log_pc_usd', title = '', palette = "viridis",
          labels = my_labels) +
  tm_facets(by = "region_fac", nrow = 2, free.coords = TRUE) +
  tm_layout(
    main.title = "Per Capita GDP by Region",
    main.title.size = 1,
    main.title.position = "center",
    legend.outside.position = "right"
  )



# NOTE: The free.coords argument of tm_facets() is set to TRUE. If it was instead 
#set to FALSE, the entire map of India would appear in each facet with the given 
#region highlighted.

View(simp_sf)
glimpse(simp_sf)
# esse comando pra criar a figura em facets "cria" duas colunas novas usando o mutate
#sem de fato adicionar essas colunas em simp_sf :D
# comparar com:
# a <- simp_sf %>% 
# mutate(
#   log_pc_usd = log10(per_capita_gdp_usd),
#   region_fac = factor(region, levels = c("Northern", "Western", "Southern",
#                                          "Central", "Eastern", "Northeastern",
#                                          "Arabian Sea", "Bay of Bengal")) 
# )

### Proportional Symbols Maps----

pop_bubbles <- simp_sf %>% 
  tm_shape() +
  tm_polygons() +
  tm_bubbles(col = "gold", size = "pop_2011", 
             scale = 3, title.size = "") +
  tm_text("abb", size = "pop_2011", root = 5,
          legend.size.show = FALSE) +
  tm_layout(
    main.title = "Population (2011)",
    main.title.position = c("center"),
    main.title.size = 1,
    legend.position = c("right", "bottom")   
  )

gdp_bubbles <- simp_sf %>% 
  tm_shape() +
  tm_polygons() +
  tm_bubbles(col = "gold", size = "nominal_gdp_usd", 
             scale = 3, title.size = "") +
  tm_text("abb", size = "nominal_gdp_usd", root = 5,
          legend.size.show = FALSE) +
  tm_layout(
    main.title = "Nominal GDP (USD)",
    main.title.position = c("center"),
    main.title.size = 1,
    legend.position = c("right", "bottom") 
  )

tmap_arrange(pop_bubbles, gdp_bubbles)

### geom_sf in ggplot2----

# ggplot2 requires tidy data. Since spatial dataframes defined in the sf package are dataframes, it makes sense 
#that we could expect to use ggplot2 to visualize sf objects. 
# Recently, ggplot2 added support for sf objects with geom_sf(). The key advantage of geom_sf() is that tidyverse
#users are already familiar with ggplot2 and its wider ecosystem of add-on packages. 


# In the plot below, we want to add only the state name "Kerala" to the map. We could have done it with the 
#annotate() function, but instead we created an sf object (also a dataframe) holding only the feature we wanted 
#to annotate (Kerala). 

# In order to do this successfully, however, we first need to find the geographic center of Kerala to know the 
#point from which to draw the label. Geometric operations like calculating centroids, buffers and distance 
#require a projected CRS as opposed to a geographic CRS, and so we've done so below using st_transform(). 

#  With a geographic CRS, st_centroid() does produce a result, but it produces a warning that "st_centroid doesn't
#give correct centroids for longitude/latitude data" because it assumes attributes are constant over geometries. 
#The distance between longitudes, however, changes based on its given latitude. (Think of the distance between 
#longitudes at the equator vs. at the North Pole.)

# The question then becomes choosing an appropriate projected CRS. Viewing crs_data = rgdal::make_EPSG() shows 
#thousands of options. We also searched for "India" at EPSG.io. Ultimately we chose a CRS with EPSG code 24343, 
#which notes "# Kalianpur 1975 / UTM zone 43N" since UTM zone 43N covers Kerala. (You might also find Projection 
#Wizard useful.)

# Using this CRS, we were able to use st_transform() to project both sf objects onto the same projected CRS. 
#Once that was done, we could add the Kerala label using geom_text_repel() like we'd normally do in ggplot2. 


library(ggplot2)
library(ggrepel)

proj_sf <- simp_sf %>% 
  st_transform(crs = 24343) %>% 
  mutate(
    CENTROID = purrr::map(geometry, st_centroid),
    COORDS = purrr::map(CENTROID, st_coordinates),
    COORDS_X = purrr::map_dbl(COORDS, 1),
    COORDS_Y = purrr::map_dbl(COORDS, 2)
  )

kerala <- proj_sf %>% 
  filter(state_ut == "Kerala")

proj_sf %>%
  filter(!state_ut %in% c("Daman & Diu", "Dadra & Nagar Haveli")) %>% 
  ggplot() + 
  geom_sf(aes(fill = sex_ratio), lwd = 0) +
  geom_sf(fill = NA, color = "grey", lwd = 0.5) +
  scale_fill_viridis_c("Sex Ratio", labels = scales::comma, option = "A") +
  labs(
    title = "Sex Ratio across Indian States",
    caption = "Source: Wikipedia"
  ) +
  geom_text_repel(
    data = kerala,
    mapping = aes(x = COORDS_X, y = COORDS_Y, label = state_ut),
    nudge_x = -0.5,
    nudge_y = -1
  ) +
  scale_y_continuous(NULL) +
  scale_x_continuous(NULL) +
  theme(plot.title = element_text(hjust = 0.5)) +
  # remove graticules
  coord_sf(datum = NA) +
  theme_void()

### Dot Density Maps----
#continuar de https://atlan.com/courses/introduction-to-gis-r/lesson3-static-maps/

