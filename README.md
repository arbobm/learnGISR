# learnGISR
primeiros scripts no aprendizado de SIG no R

# fonte do aprendizado

## Livros
[Geocomputation with R - Chapter 2](https://geocompr.robinlovelace.net/spatial-class.html)

[Spatial Data Science](https://keen-swartz-3146c4.netlify.app/index.html)

See the [Relational data](https://r4ds.had.co.nz/relational-data.html) chapter if `left_join()` is unfamiliar

## Sites
[introduction-to-gis-r](https://atlan.com/courses/introduction-to-gis-r/)

[Units of Measurement for R Vectors: an Introduction](https://cran.r-project.org/web/packages/units/vignettes/units.html#setting-units-unit-conversion)

For more information on simplification, see [Section 5.2.1](https://geocompr.robinlovelace.net/geometric-operations.html) of *Geocomputation with R*.

# Resumo copiado de [introduction-to-gis-r Lesson 2](https://atlan.com/courses/introduction-to-gis-r/lesson2-manipulating-geospatial-data/)

## Chapter 2

### Final Thoughts

After briefly introducing the context of using R as a GIS, this lesson showed how the `sf` package creates a class structure for storing geospatial and attribute data together in an object that fits into a tidyverse workflow. We can clearly see the benefits of this structure when it comes to manipulating a spatial dataframe with our familiar `dplyr` verbs.

Now that you know how to manipulate geospatial data, the natural next step is visualization or mapping. Here again, we'll see the benefit of a tidy workflow, now that `ggplot2`’s `geom_sf()` is available to us. Though, as you'll see in the next lesson, `ggplot2` is just one of many excellent package options when it comes to visualizing geospatial data in R. 