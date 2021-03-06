% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/cleandata.R
\name{eq_clean_data}
\alias{eq_clean_data}
\title{Clean the raw NOAA earthquake data to interface well with the other functions in this package
The NOAA earthquake database that is included with this package is already clean and ready to use with the other functions.  
However, that data is frozenas of July 1, 2017.  More up-to-date data can be cleaned up using this function}
\usage{
eq_clean_data(raw_data)
}
\arguments{
\item{raw_data}{A data frame containing the raw NOAA earthquake data.
The function assumes the data has been read in using \code{readr::read_delim} with the option \code{delim = '\\t'}}
}
\value{
A data frame of cleaned data, ready to be used in the visualization functions included in this package
}
\description{
Clean the raw NOAA earthquake data to interface well with the other functions in this package
The NOAA earthquake database that is included with this package is already clean and ready to use with the other functions.  
However, that data is frozenas of July 1, 2017.  More up-to-date data can be cleaned up using this function
}
\details{
The raw data is available \href{https://www.ngdc.noaa.gov/nndc/struts/form?t=101650&s=1&d=1}{here}
The data cleanup does the following:
1. create DATE, and make sure it is date class
2. make sure LONGITUDE and LATTITUDE are numeric; drop NA lat/lon
3. clean location name
4. make sure magnatude measures are numeric
5. make sure deaths are numeric
}
\examples{
# assumes the raw NOAA Significant Earthquake Database is available in
# your working directory as 'raw_NOAA.txt'
\dontrun{
clean_NOAA <- eq_clean_data(readr::read_delim('raw_NOAA.txt', delim = '\\\\t'))
}
}
