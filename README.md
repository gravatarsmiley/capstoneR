<!-- README.md is generated from README.Rmd. Please edit that file -->
noaa
====

[![Build Status]]
https://travis-ci.org/gravatarsmiley/capstoneR.svg?branch=master

This package was created as a capstone project for the Coursera course "Mastering Software Development in R". The package contains functions to clean and visualize earthquake data from the "U.S. National Oceanographic and Atmospheric Administration" "Significant Earthquakes" data set. The visualizations include plotting the earthquakes along a timeline as well as on an interactive map.


Example
-------

The NOAA "Significant Earthquakes" data set is available for download here: <https://www.ngdc.noaa.gov/nndc/struts/form?t=101650&s=1&d=1>
Assuming that you have downloaded the relevant data set from the link above, the following code reads in the data set and cleans it so that it is ready to be used in the noaa package. In the example we load in the complete data set included in the noaa package, but by changing the "file" argument to "read\_delim" we can load in a different data set.

``` r
library(magrittr)
library(capstoneR)
eq_data <- readr::read_delim(file = system.file("extdata", "signif.txt", package="capstoneR"), delim = "\t") %>%
  eq_clean_data() %>% eq_location_clean()
```

The following code filters the loaded data set and visualizes it on a timeline, stratified by country with the color aesthetic indicating the number of deaths and the size aesthetic indicating the magnitude of the earthquakes.

``` r
p <- eq_data %>%
     dplyr::filter(YEAR >= 1900, !is.na(DEATHS), !is.na(EQ_MAG_ML),
                  COUNTRY %in% c("CHINA", "USA")) %>%
  ggplot2::ggplot() +
  theme_time() +
  geom_timeline(ggplot2::aes(x = DATE,
                             y = COUNTRY,
                             colour = DEATHS,
                             size = EQ_MAG_ML)) +
  ggplot2::labs(x = "DATE", color = "# deaths", size = "Richter scale value")
gt <- ggplot2::ggplot_gtable(ggplot2::ggplot_build(p))
gt$layout$clip[gt$layout$name=="panel"] = "off"
grid::grid.draw(gt)
```

The code below does the same timeline based visualization, but with location labels on the 5 earthquakes with the largest magnitude for each country.

``` r
p <- readr::read_delim(file = system.file("extdata", "signif.txt", package="capstoneR"),
                                         delim = "\t") %>%
  eq_clean_data() %>% eq_location_clean() %>%
  dplyr::filter(YEAR >= 1900, !is.na(DEATHS), !is.na(EQ_MAG_ML),
                COUNTRY %in% c("CHINA", "USA")) %>%
  ggplot2::ggplot(ggplot2::aes(x = DATE,
                               y = COUNTRY,
                               colour = DEATHS,
                               size = EQ_MAG_ML)) +
  theme_time() +
  geom_timeline() +
  geom_timeline_label(ggplot2::aes(label = LOCATION_NAME, n_max = 5)) +
  ggplot2::labs(x = "DATE", color = "# deaths", size = "Richter scale value")
gt <- ggplot2::ggplot_gtable(ggplot2::ggplot_build(p))
gt$layout$clip[gt$layout$name=="panel"] = "off"
grid::grid.draw(gt)
```

The noaa package can also be used to visualize the earthquake data set on an interactive map. The following code produces an htmlwidget containing a navigable map with earthquakes marked by circles. The radius of the circle depends on the magnitude of the earthquake, and the circles display information about the earthquake when clicked. This information includes the date of the earthquake, the magnitude of the earthquake and the numbers of deaths caused by the earthquake.

``` r
readr::read_delim(file = system.file("extdata", "signif.txt", package="capstoneR"), delim = "\t") %>%
eq_clean_data() %>%
dplyr::filter(COUNTRY == "MEXICO" & lubridate::year(DATE) >= 2000) %>%
dplyr::mutate(popup_text = eq_create_label(.)) %>%
eq_map(annot_col = "popup_text")
```
