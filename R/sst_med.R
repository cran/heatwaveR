#' NOAA Optimally Interpolated (OI) v2.1 daily 1/4 degree SST for the Mediterranean region.
#'
#' A dataset containing the sea surface temperature (in degrees Celsius)
#' and date for the Mediterranean region from 1982-01-01 to 2019-12-31.
#'
#' lon/lat: 9/43.5
#'
#' @format A dataframe with 13879 rows and 2 variables:
#' \describe{
#'   \item{t}{date, as.Date() format}
#'   \item{temp}{SST, in degrees Celsius}
#'   ...
#' }
#' @source \url{https://www.ncdc.noaa.gov/oisst}
"sst_Med"
