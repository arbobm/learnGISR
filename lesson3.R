## LESSON 3 - Creating Static Maps in R
## https://atlan.com/courses/introduction-to-gis-r/lesson3-static-maps/
## introduction-to-gis-r

# This lesson introduces how to use some of the most well-known R packages to create static maps, such as tmap and ggplot2.
# We’ll also explore a few other packages like cartogram, geogrid and geofacet for some more unique spatial visualizations. 

## pacotes necessários---- 
# install.packages("tmap") #demoradinho
# install.packages("ggplot2")
# install.packages("cartogram")
# install.packages("geogrid")
# install.packages("geofacet")

### Resources on Visualizing Geospatial Data----

# Before diving into different R packages for mapping, let's review a few excellent resources that will help you get started. 

# Recent advances in software have made many different types of geospatial data visualizations — such as choropleths, dot density maps and cartograms — easily available. However, the correct visualization often begins with the type of data you have. 

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
# retomar em https://atlan.com/courses/introduction-to-gis-r/lesson3-static-maps/

