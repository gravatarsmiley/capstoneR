#' Create an interactive map of historical earthquakes
#' Create an interactive map of historical earthquakes using the NOAA earthquake database included with this package.  
#' Earthquakes are plotted as circles with their radii proportional to the magnatude of the earthquakes. 
#' Optionally, labels can be passed which will pop up when the earthquake is clicked on the map
#' This function uses of the popular \code{leaflet} package, which creates interactive html maps within R
#' @param data A dataframe with earthquake location columns \code{LATITUDE} and \code{LONGITUDE}, and magnatude column \code{EQ_PRIMARY}.  
#' The included EQ_NOAA dataframe is already correctly formateed, and is intended to be used with this function
#' @param annot_col The character name of the column in \code{data} containing a character vector of optional popup text to be shown when the earthquake is clicked.  
#' Defaults to NULL, where no label is shown
#' @return An html map object, to which further Leaflet objects can be added
#' @examples
#' \dontrun{
#' data('clean_NOAA')
#' clean_NOAA %>%
#'   dplyr::filter(COUNTRY == "MEXICO" & lubridate::year(DATE) >= 2000) %>%
#'   eq_map(annot_col = "DATE")
#' data('clean_NOAA')
#' clean_NOAA %>%
#'   dplyr::filter(COUNTRY == "MEXICO" & lubridate::year(DATE) >= 2000) %>%
#'   dplyr::mutate(popup_text = eq_create_label(.)) %>%
#'   eq_map(annot_col = "DATE")
#' }
#' @seealso \pkg{leaflet}
#' @references NOAA earthquake database: \url{https://www.ngdc.noaa.gov/nndc/struts/form?t=101650&s=1&d=1}
#' @export
#' @importFrom dplyr mutate filter
#' @importFrom magrittr "%>%"
#' @import leaflet
eq_map <- function(data, annot_col = NULL) {
    mp <- data %>% leaflet::leaflet() %>%
        leaflet::addTiles()
    if(!is.null(annot_col)) {
        mp <- mp %>% leaflet::addCircleMarkers(lng = ~LONGITUDE,
                         lat = ~LATITUDE,
                         radius = ~EQ_PRIMARY,
                         weight = 1,
                         popup = data[[annot_col]])
    } else {
        mp <- mp %>% leaflet::addCircleMarkers(lng = ~LONGITUDE,
                                      lat = ~LATITUDE,
                                      radius = ~EQ_PRIMARY,
                                      weight = 1)
    }
}
#' Creates a label for leaflet map
#' This function creates a label for the \code{leaflet} map based on location name, magnitude and casualties from NOAA earthquake data
#' @param data A data frame containing cleaned NOAA earthquake data
#' @return A character vector with labels
#' @details The input \code{data.frame} needs to include columns LOCATION_NAME,
#' EQ_PRIMARY and TOTAL_DEATHS with the earthquake location, magintude and total casualties respectively.
#' @export
#' @examples
#' \dontrun{
#' eq_create_label(data)
#' }
eq_create_label <- function(data) {
     popup_text <- with(data, {
          part1 <- ifelse(is.na(LOCATION_NAME), "",
                          paste("<strong>Location:</strong>",
                                LOCATION_NAME))
          part2 <- ifelse(is.na(EQ_PRIMARY), "",
                          paste("<br><strong>Magnitude</strong>",
                                EQ_PRIMARY))
          part3 <- ifelse(is.na(TOTAL_DEATHS), "",
                          paste("<br><strong>Total deaths:</strong>",
                                TOTAL_DEATHS))
          paste0(part1, part2, part3)
     })
}
