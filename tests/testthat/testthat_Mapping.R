library(capstoneR)
library(readr)
library(lubridate)
library(ggplot2)
library(dplyr)
library(leaflet)
context("Testing mapping functions")

data(clean_NOAA)

map <- clean_NOAA %>%           
		  dplyr::filter(COUNTRY == "MEXICO" & lubridate::year(DATE) >= 2000) %>%
          dplyr::mutate(popup_text = eq_create_label(.)) %>%
          eq_map(annot_col = "popup_text")
     expect_is(map, "leaflet")

test_that("A leaflet object is returned",{
    expect_is(map, "leaflet")
})

test_that("eq_create_label has the same length as input df", {
     expect_equal(length(eq_create_label(clean_NOAA)), dim(clean_NOAA)[1])
})
