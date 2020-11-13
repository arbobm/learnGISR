## LESSON 4 - Creating Animated and Interactive Maps in R
## https://atlan.com/courses/introduction-to-gis-r/lesson4-animated-interactive-maps/


### pacotes----

library(tidyverse)
library(sf)
library(tmap)
library(magrittr)
library(gganimate)
# install.packages("transformr")
# install.packages("ggiraph")
# install.packages("leaflet.extras")
# install.packages("plotly")
# install.packages("listviewer")
# install.packages("crosstalk")
# install.packages("DT")


### Animated Maps----

# Animated maps are particularly well-suited for spatio-temporal data as they can show change of a 
# variable over time, but they certainly have other uses as well.

# This lesson introduces two methods for making animated maps: animated tmaps and the gganimate package. 
# We’ll also need the packages and objects from the previous lesson. 

simp_sf <- readRDS("simp_sf.rds")

states_sf <- simp_sf %>% 
  filter(!type == "Union Territory")

proj_sf <- simp_sf %>% 
  st_transform(crs = 24343) %>% 
  mutate(
    CENTROID = purrr::map(geometry, st_centroid),
    COORDS = purrr::map(CENTROID, st_coordinates),
    COORDS_X = purrr::map_dbl(COORDS, 1),
    COORDS_Y = purrr::map_dbl(COORDS, 2)
  )

### Animation with tmap----

# the simplest way to create basic animated maps is through a slight modification to a faceted tmap plot.
# Simply change the tm_facets() argument by to along. (ImageMagick is required.) 
# Then, providing the generated output to the tmap_animation() function loops each faceted plot into a 
# gif or mpeg animation. You can specify further details like the delay of each frame and the output 
# dimensions. 

pc_gdp_anim <- simp_sf %>%
  filter(!state_ut %in% c("Lakshadweep", "Andaman & Nicobar Islands")) %>% 
  mutate(
    log_pc_usd = log10(per_capita_gdp_usd),
    region_fac = factor(region, levels = c("Northern", "Western", "Southern",
                                           "Central", "Eastern", "Northeastern")) 
  ) %>%
  tm_shape() +
  tm_fill(col = 'log_pc_usd', title = 'Per Capita GDP (USD$)', 
          palette = "viridis",
          labels = my_labels) +
  tm_borders(col = "white") +
  tm_facets(along = "region_fac", free.coords = FALSE) +
  tm_layout(main.title.size = 1)

tmap_animation(pc_gdp_anim, filename = "pc_gdp_anim.gif", 
               delay = 200, restart.delay = 200)
## não consegui executar por não ter entendido o que é e como instala o ImageMagick. ver https://atlan.com/courses/introduction-to-gis-r/lesson4-animated-interactive-maps/

### Animation with gganimate----

# more robust animations (not only geospatial)
# The package allows not just for looping of facets but also a huge array of possibilities. 
# This leads to some really creative plots, as found in the package’s wiki. https://github.com/thomasp85/gganimate/wiki

# After binding together transformed data sets into one object, we can achieve the animation below with 
# just one additional line to our plot: transition_states(), which mimics ggplot2::facet_wrap() by
# splitting the data into multiple panels, tweening between defined states and pausing at each state for 
# a specified period. 

states <- c(
  'Original',
  'Continuous Cartogram Weighted by Nominal GDP',
  'Dorling Cartogram Weighted by Nominal GDP',
  'Hexagonal Tiling'
)

# cartograms, hexbin created in previous lesson

proj_sf$state <- states[1]
ccart_gdp_sf$state <- states[2]
dorling_gdp_sf$state <- states[3]
hex_result$state <- states[4]

nom_gdp_all <- rbind(proj_sf, 
                     ccart_gdp_sf[, names(proj_sf)],
                     dorling_gdp_sf[, names(proj_sf)],
                     hex_result[, names(proj_sf)])
nom_gdp_all$state <- factor(nom_gdp_all$state, levels = states)

library(transformr)

a<- ggplot(nom_gdp_all) + 
  geom_sf(aes(fill = nominal_gdp_usd / 1e9, group = state_ut)) + 
  scale_fill_viridis_c(labels = scales::dollar) +
  coord_sf(datum = NA) +
  theme_void() + 
  theme(legend.position = 'bottom', 
        legend.text = element_text(angle = 30, hjust = 1)) + 
  labs(title = 'Showing {closest_state}', 
       fill = 'Nominal GDP (USD$ Billion)') +
  transition_states(state, 2, 1)

animate(a, duration = 5, fps = 20, width = 200, height = 200, renderer = gifski_renderer())
# a linha de comando acima eu incluí porque sem ela não estava funcionando. incusão baseada na sugestão de https://github.com/thomasp85/gganimate/issues/335
anim_save("nom_gdp_anim.gif")

### Interactive Maps----

# https://atlan.com/courses/introduction-to-gis-r/lesson4-animated-interactive-maps/

#### Interactivity with ggiraph----

# Let’s start simple. In some cases, we may only want to add a minimal amount of interactivity 
# (such as a hover or tooltip effect). In that case, we could turn to the plotly or ggiraph packages. 

# The plotly package is a popular choice for adding interactivity to ggplot2 plots. 
# The ggplotly() function handles geospatial data in the same way as non-spatial data.

# For simple interactivity, you might try the ggiraph package from David Gohel. 
# The basic idea is to pass ggplot() an interactive geom in place of a traditional geom. 
# In the case of maps, this means using geom_sf_interactive() instead of geom_sf().

library(ggiraph)

tooltip_css <- "background-color:gray;color:white;padding:5px;border-radius:5px;font-family:sans-serif;font-size:12px;"

gg_sex <- simp_sf %>%
  mutate(
    tip = str_c(
      "<b>", state_ut, " : ", sex_ratio, "</b>",
      "</span></div>")
  ) %>% 
  filter(!state_ut %in% c("Daman & Diu", "Dadra & Nagar Haveli")) %>% 
  ggplot() + 
  geom_sf_interactive(aes(fill = sex_ratio, 
                          tooltip = tip, data_id = state_ut)) +
  geom_sf(fill = NA, color = "grey", lwd = 0.5) +
  scale_fill_viridis_c("Sex Ratio", labels = scales::comma, option = "A") +
  labs(
    title = "Sex Ratio across Indian States",
    caption = "Source: Wikipedia"
  ) +
  coord_sf(datum = NA) +
  theme_void() +
  theme(plot.title = element_text(hjust = 0.5))

ggiraph(ggobj = gg_sex, 
        hover_css = "cursor:pointer;stroke-width:5px;fill-opacity:0.8;",
        tooltip_extra_css = tooltip_css, tooltip_opacity = 0.75)

#### Interactive tmaps----

# ability to layer geospatial data (such as points or polygons) on top of base maps that depict physical 
# or human geography. 
# Perhaps the easiest way to achieve this advantage is by plotting tmap objects in tmap’s "view" mode. 

# By default, tmap_mode() is set to "plot", but changing this argument to "view" can make any tmap object 
# an interactive plot. This option builds on top of Leaflet, which we'll cover below. 

tmap_mode("view")
states_sf %>% 
  mutate(
    gdp_density = gdp_density_usd_km2 / 1e6,
    label = str_c(state_ut, ": ", gdp_density)
  ) %>% 
  select(label, everything()) %>%
  tm_shape() +
  tm_fill(col = 'gdp_density', title = "USD$(mil)/sq.km") +
  tm_borders(lwd = 0.5)

# However, interactive tmap objects, particularly arranged tmap objects, seem to be difficult to modify 
# after creation. For instance, the tooltip took the first column by default, so we edited the first 
# column to include the tooltip we wanted. Legends also seemed difficult to move from the top right for 
# some reason. Because of small issues like this, you might be better off directly building in Leaflet for more polished projects. 

### Interactivity with mapview----

# Its primary role is to create quick interactive geospatial visualizations, rather than presentation-quality visualizations — but its functionality may be growing.

library(mapview)
mapview(
  simp_sf,
  zcol = c("state_ut", "pop_2011", "per_capita_gdp_usd", 
           "density_km2", "sex_ratio"),
  legend = FALSE,
  hide = TRUE
)


### Interactivity with Leaflet----

# Both mapview and the interactive mode of tmap rely on Leaflet under the hood. 
# Eventually, you'll likely want to build directly in Leaflet. 

# In order to achieve this effect in Leaflet, we first need to use the st_cast() function to convert 
# our earlier multi-points into individual points because Leaflet doesn't support multi-point objects 
# at this time. We also need to transform the projected CRS to a geographic CRS
# (longitude and latitude coordinates) for plotting in Leaflet. 
# Then we establish each respective layer as a "group". Remember, here one dot equals one lakh (100,000) people. 

library(leaflet)
library(leaflet.extras)

# multipoints are not supported so cast to points
my_points <- st_cast(points, "POINT")

# transform to geo crs for leaflet mapping
geo_points <- my_points %>% 
  st_transform(crs = 4326)

# create data for different groups
rural <- geo_points %>% filter(pop == "rural_pop")
urban <- geo_points %>% filter(pop == "urban_pop")
state_lines <- sf::st_geometry(simp_sf)

leaflet() %>% 
  addProviderTiles("CartoDB.Positron") %>%
  addResetMapButton() %>% 
  addPolylines(
    data = state_lines,
    color = "black", 
    weight = 1,
    opacity = 1,
  ) %>%
  addCircleMarkers(
    data = rural,
    color = "#F8766D",
    radius = 1,
    stroke = FALSE,
    fillOpacity = 0.5,
    group = "Rural"
  ) %>%
  addCircleMarkers(
    data = urban,
    color = "#00BFC4",
    radius = 1,
    stroke = FALSE,
    fillOpacity = 0.5,
    group = "Urban"
  ) %>%
  addLayersControl(
    overlayGroups = c("Rural", "Urban", "Rural Population"),
    options = layersControlOptions(collapsed = FALSE)
  )

#### Interactivity with plotly----

# ler https://atlan.com/courses/introduction-to-gis-r/lesson4-animated-interactive-maps/

library(plotly)
plot_ly(states_sf,
        split = ~state_ut, 
        color = ~density_km2,
        text = ~paste0(state_ut, ": ", density_km2),
        hoverinfo = "text",
        hoveron = "fill",
        alpha = 0.8,
        showlegend = FALSE) %>% 
  layout(title = "Population Density across Indian States")


library(listviewer)
library(crosstalk)
library(DT)

simp_sd <- SharedData$new(
  simp_sf %>% 
    select(state_ut, pop_2011, decadal_growth, region) %>% 
    mutate(pop_2011 = format(simp_sf$pop_2011, big.mark = ",")), 
  ~region)

bscols(
  plot_mapbox(
    simp_sd, 
    text = ~state_ut, hoverinfo = "text"
  ) %>% 
    layout(title = "Filter Table by Region via Point Selection"),
  DT::datatable(
    simp_sd,
    rownames = FALSE,
    colnames = c('State/UT','Population','Pop. Growth','Region','geometry'),
    options = list(
      autoWidth = FALSE,
      columnDefs = list(
        list(width = '50px', className = 'dt-left', targets = 0:4)
      )
    )
  )
)
## precisa de um MAPBOX_TOKEN

simp_sd <- SharedData$new(simp_sf)

bscols(
  plot_mapbox(
    simp_sd,
    split = ~state_ut,
    color = I("grey"),
    text = ~paste0(state_ut, ": ", sex_ratio),
    hoverinfo = "text",
    alpha = 0.8,
    showlegend = FALSE
  ) %>%
    highlight(dynamic = TRUE, persistent = TRUE),
  plot_ly(simp_sd, x = ~sex_ratio) %>% 
    add_histogram(xbins = list(start = 600, end = 1100, size = 25)) %>%
    layout(
      barmode = "overlay", 
      xaxis = list(title = "Sex Ratio"),
      yaxis = list(title = "No. of States/UTs")
    ) %>% 
    highlight("plotly_selected", persistent = TRUE)
)

### Mapping Applications in Shiny----

#The Basics of Shiny Apps

## Essentially, Shiny apps have two parts: a front end and a back end. When creating a 
## Shiny app, you can choose to build it as a single file (in which case, the front end 
## and back end are housed in two functions, ui() and server()) or two files (in which case, 
## the front end and back end are found in separate files, ui.R and server.R). 

## ui.R controls the app’s appearance
## server.R contains the logic that transforms a list of user inputs, such as dropdown 
## menus or radio buttons, into various kinds of outputs, like plots or tables. 

## Beyond these minimum two files, larger projects often involve a few other important 
## components. One is a separate data folder that holds all of the data read into the app. 
## Another is a file, perhaps named global.R, that reads in data files, sets global variables, 
## and contains functions to be used in server.R. 

## Lastly, you might add a styles.css file for custom styling. I chose to add 
## includeCSS(styles.css) inside the header tag within my ui.R file. This allowed us to
## override any of the app’s default styling in a separate file without distracting from 
## the structure of ui.R. 



