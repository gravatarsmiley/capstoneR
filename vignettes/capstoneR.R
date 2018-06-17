## ----echo = FALSE, message = FALSE, warning = FALSE----------------------
knitr::opts_chunk$set(collapse = TRUE, comment = "#>")
options(tibble.print_min = 4L, tibble.print_max = 4L)
library(capstoneR)
library(ggplot2)
library(dplyr)
library(readr)
library(leaflet)
library(lubridate)

## ------------------------------------------------------------------------
data('clean_NOAA')

## ----eval = FALSE--------------------------------------------------------
#  clean_NOAA <- readr::read_delim('signif.txt',delim = '\t') %>%
#      eq_clean_data()

## ---- fig.width = 7, fig.height = 4--------------------------------------
filter(clean_NOAA, COUNTRY == "USA") %>% ggplot(aes(x = DATE)) + 
    geom_timeline(x_min = ymd("2000-01-01"), x_max = ymd("2017-07-01")) +
    theme_minimal() + theme(panel.grid.major.x = element_blank()) +
    theme(axis.text.y  = element_blank()) +
    theme(panel.grid.minor.x = element_blank()) +
    scale_y_continuous(breaks = 1, limits = c(0.5, 2)) +
    labs(x = "Date")


## ---- fig.width = 7, fig.height = 4--------------------------------------
filter(clean_NOAA, COUNTRY == "USA" | COUNTRY == "CANADA") %>% 
    ggplot(aes(x = DATE, y = COUNTRY, size = EQ_PRIMARY)) + 
    geom_timeline(x_min = ymd("2000-01-01"), x_max = ymd("2017-07-01")) +
    theme_minimal() + theme(panel.grid.major.x = element_blank()) +
    theme(panel.grid.minor.x = element_blank()) + theme(legend.position = "bottom") +
    theme(legend.key.size = unit(0.078, 'npc')) +
    labs(x = "Date", y = "Country", size = "Magnatude")

## ---- fig.width = 7, fig.height = 5--------------------------------------
xmin <- ymd("2000-01-01")
xmax <- ymd("2017-07-01")
filter(clean_NOAA, COUNTRY == "CANADA" | COUNTRY == "USA") %>% 
    ggplot(aes(x = DATE, y = COUNTRY, size = EQ_PRIMARY)) + 
    geom_timeline(x_min = xmin, x_max = xmax) +
    geom_timeline_label(aes(label = LOCATION_NAME, magnatude = EQ_PRIMARY),
                        x_min = xmin, x_max = xmax, top_x_mag = 5) +
    theme_minimal() + theme(panel.grid.major.x = element_blank()) +
    theme(panel.grid.minor.x = element_blank()) + theme(legend.position = "bottom") +
    theme(legend.key.size = unit(0.078, 'npc')) +
    labs(x = "Date", y = "Country", size = "Magnatude")

## ---- fig.width = 7, fig.height = 5--------------------------------------
map <- clean_NOAA %>%           
		  dplyr::filter(COUNTRY == "MEXICO" & lubridate::year(DATE) >= 2000) %>%
          dplyr::mutate(popup_text = eq_create_label(.)) %>%
          eq_map(annot_col = "popup_text")
map		  

