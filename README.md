# learnGISR
primeiros scripts no aprendizado de SIG no R

[FREE ONLINE COURSE - Introduction to GIS - Manipulating and Mapping Geospatial Data in R](https://atlan.com/courses/introduction-to-gis-r/overview/)

# fonte do aprendizado

## Livros
[Geocomputation with R - Chapter 2](https://geocompr.robinlovelace.net/spatial-class.html)

[Spatial Data Science](https://keen-swartz-3146c4.netlify.app/index.html)

See the [Relational data](https://r4ds.had.co.nz/relational-data.html) chapter if `left_join()` is unfamiliar

 Below are two excellent open source resources on the principles of data visualization. Both include chapters on geospatial data visualization.

[Data Visualization: A practical introduction](https://socviz.co/)

[Maps chapter](http://socviz.co/maps.html#maps)

[Fundamentals of Data Visualization](https://serialmentor.com/dataviz/)


[Making Maps with R chapter](https://geocompr.robinlovelace.net/adv-map.html) of the previously-mentioned [*Geocomputation with R*](https://geocompr.robinlovelace.net/index.html)

[color scales](https://serialmentor.com/dataviz/color-basics.html) 

[color pitfalls](https://serialmentor.com/dataviz/color-pitfalls.html) 


## Sites
[introduction-to-gis-r](https://atlan.com/courses/introduction-to-gis-r/)

[Units of Measurement for R Vectors: an Introduction](https://cran.r-project.org/web/packages/units/vignettes/units.html#setting-units-unit-conversion)

For more information on simplification, see [Section 5.2.1](https://geocompr.robinlovelace.net/geometric-operations.html) of *Geocomputation with R*.

[Processamento de dados usando *dplyr*](https://vanderleidebastiani.github.io/tutoriais/Processamento_de_dados_usando_dplyr.html)

Bhaskar V. Karambelkar’s [tutorial](https://bhaskarvk.github.io/user2017.geodataviz/) at useR 2017 on "Geospatial Data Visualization in R" 

[`tmap` documentation](https://github.com/mtennekes/tmap)

[Cartography Guide - A short, friendly guide to basic principles of map design](https://www.axismaps.com/guide/)

# Resumo copiado de [introduction-to-gis-r Lesson 2](https://atlan.com/courses/introduction-to-gis-r/lesson2-manipulating-geospatial-data/)

## Chapter 2

### Final Thoughts

After briefly introducing the context of using R as a GIS, this lesson showed how the `sf` package creates a class structure for storing geospatial and attribute data together in an object that fits into a tidyverse workflow. We can clearly see the benefits of this structure when it comes to manipulating a spatial dataframe with our familiar `dplyr` verbs.

Now that you know how to manipulate geospatial data, the natural next step is visualization or mapping. Here again, we'll see the benefit of a tidy workflow, now that `ggplot2`’s `geom_sf()` is available to us. Though, as you'll see in the next lesson, `ggplot2` is just one of many excellent package options when it comes to visualizing geospatial data in R. 

