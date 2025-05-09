## ----global_options, include = FALSE------------------------------------------
knitr::opts_chunk$set(fig.width = 8, fig.height = 3, fig.align = 'centre',
                      echo = TRUE, warning = FALSE, message = FALSE,
                      eval = FALSE, tidy = FALSE)

## ----setup--------------------------------------------------------------------
# # The packages we will need
# # install.packages("dplyr")
# # install.packages("lubridate")
# # install.packages("ggplot2")
# # install.packages("tidync")
# # install.packages("doParallel")
# # install.packages("rerddap")
# # install.packages("plyr") # Note that this library should never be loaded, only installed
# 
# # The packages we will use
# library(dplyr) # A staple for modern data management in R
# library(lubridate) # Useful functions for dealing with dates
# library(ggplot2) # The preferred library for data visualisation
# library(tidync) # For easily dealing with NetCDF data
# library(rerddap) # For easily downloading subsets of data
# library(doParallel) # For parallel processing

## ----erddap-info--------------------------------------------------------------
# # The information for the NOAA OISST data
# rerddap::info(datasetid = "ncdcOisst21Agg_LonPM180", url = "https://coastwatch.pfeg.noaa.gov/erddap/")
# 
# # Note that there is also a version with lon values from 0 yo 360
# rerddap::info(datasetid = "ncdcOisst21Agg", url = "https://coastwatch.pfeg.noaa.gov/erddap/")

## ----download-func------------------------------------------------------------
# # This function downloads and prepares data based on user provided start and end dates
# OISST_sub_dl <- function(time_df){
#   OISST_dat <- rerddap::griddap(datasetx = "ncdcOisst21Agg_LonPM180",
#                                 url = "https://coastwatch.pfeg.noaa.gov/erddap/",
#                                 time = c(time_df$start, time_df$end),
#                                 zlev = c(0, 0),
#                                 latitude = c(-40, -35),
#                                 longitude = c(15, 21),
#                                 fields = "sst")$data |>
#     dplyr::mutate(time = base::as.Date(stringr::str_remove(time, "T12:00:00Z"))) |>
#     dplyr::rename(t = time, temp = sst, lon = longitude, lat = latitude) |>
#     dplyr::select(lon, lat, t, temp) |>
#     stats::na.omit()
# }

## ----year-index---------------------------------------------------------------
# # Date download range by start and end dates per year
# dl_years <- data.frame(date_index = 1:5,
#                        start = c("1982-01-01", "1990-01-01",
#                                  "1998-01-01", "2006-01-01", "2014-01-01"),
#                        end = c("1989-12-31", "1997-12-31",
#                                "2005-12-31", "2013-12-31", "2019-12-31"))

## ----download-data------------------------------------------------------------
# # Download all of the data with one nested request
# # The time this takes will vary greatly based on connection speed
# base::system.time(
#   OISST_data <- dl_years |>
#     dplyr::group_by(date_index) |>
#     dplyr::group_modify(~OISST_sub_dl(.x)) |>
#     dplyr::ungroup() |>
#     dplyr::select(lon, lat, t, temp)
# ) # 518 seconds, ~100 seconds per batch

## ----SA-visual----------------------------------------------------------------
# OISST_data |>
#   dplyr::filter(t == "2019-12-01") |>
#   ggplot2::ggplot(aes(x = lon, y = lat)) +
#   ggplot2::geom_tile(aes(fill = temp)) +
#   # ggplot2::borders() + # Activate this line to see the global map
#   ggplot2::scale_fill_viridis_c() +
#   ggplot2::coord_quickmap(expand = F) +
#   ggplot2::labs(x = NULL, y = NULL, fill = "SST (°C)") +
#   ggplot2::theme(legend.position = "bottom")

## ----prep-data----------------------------------------------------------------
# # Save the data as an .Rds file because it has a much better compression rate than .RData
# base::saveRDS(OISST_data, file = "~/Desktop/OISST_vignette.Rds")

## ----NOAA-info----------------------------------------------------------------
# # First we tell R where the data are on the interwebs
# OISST_base_url <- "https://www.ncei.noaa.gov/data/sea-surface-temperature-optimum-interpolation/v2.1/access/avhrr/"
# # Note that one may go to this URL in any web browser to manually inspect the files
# 
# # Now we create a data.frame that contains all of the dates we want to download
#   # NB: In order to change the dates download changes the dates in the following line
# OISST_dates <- base::data.frame(t = seq(as.Date("2019-12-01"), as.Date("2019-12-31"), by = "day"))
# 
# # To finish up this step we add some text to those dates so they match the OISST file names
# OISST_files <- OISST_dates |>
#   dplyr::mutate(t_day = base::gsub("-", "", t),
#                 t_month = base::substr(t_day, 1, 6),
#                 t_year = lubridate::year(t),
#                 file_name = base::paste0(OISST_base_url, t_month, "/", "oisst-avhrr-v02r01.", t_day ,".nc"))

## ----NOAA-dl------------------------------------------------------------------
# # This function will go about downloading each day of data as a NetCDF file
# # Note that this will download files into a 'data/OISST' folder in the root directory
#   # If this folder does not exist it will create it
#   # If it does not automatically create the folder it will need to be done manually
#   # The folder that is created must be a new folder with no other files in it
#   # A possible bug with netCDF files in R is they won't load correctly from
#   # existing folders with other file types in them
# # This function will also check if the file has been previously downloaded
#   # If it has it will not download it again
# OISST_url_daily_dl <- function(target_URL){
#   base::dir.create("~/data/OISST", showWarnings = F)
#   file_name <- base::paste0("~/data/OISST/",base::sapply(base::strsplit(target_URL, split = "/"), "[[", 10))
#   if(!base::file.exists(file_name)) utils::download.file(url = target_URL, method = "libcurl", destfile = file_name)
# }
# 
# # The more cores used, the faster the data may be downloaded
#   # It is best practice to not use all of the cores on one's machine
#   # The laptop on which I am running this code has 8 cores, so I use 7 here
# doParallel::registerDoParallel(cores = 7)
# 
# # And with that we are clear for take off
# base::system.time(plyr::l_ply(OISST_files$file_name, .fun = OISST_url_daily_dl, .parallel = T)) # ~15 seconds
# 
# # In roughly 15 seconds a user may have a full month of global data downloaded
# # This scales well into years and decades, and is much faster with more cores
# # Download speeds will also depend on the speed of the users internet connection

## ----NOAA-load----------------------------------------------------------------
# # This function will load and subset daily data into one data.frame
# # Note that the subsetting by lon/lat is done before the data are loaded
#   # This means it will use much less RAM and is viable for use on most laptops
#   # Assuming one's study area is not too large
# OISST_load <- function(file_name, lon1, lon2, lat1, lat2){
#       OISST_dat <- tidync::tidync(file_name) |>
#         tidync::hyper_filter(lon = dplyr::between(lon, lon1, lon2),
#                              lat = dplyr::between(lat, lat1, lat2)) |>
#         tidync::hyper_tibble(select_var = c("sst"), drop = FALSE) |>
#         dplyr::select(lon, lat, time, sst) |>
#         dplyr::rename(t = time, temp = sst) |>
#         dplyr::mutate(t = as.Date(t),
#                       lon = as.numeric(lon),
#                       lat = as.numeric(lat))
#       return(OISST_dat)
# }
# 
# # Locate the files that will be loaded
# OISST_files <- dir("~/data/OISST", full.names = T)
# 
# # Load the data in parallel
# OISST_dat <- plyr::ldply(.data = OISST_files[1:365], .fun = OISST_load, .parallel = T,
#                          lon1 = 270, lon2 = 320, lat1 = 30, lat2 = 50)
# 
# # It should only take a few seconds to load one month of data depending on the size of the lon/lat extent chosen

## ----NOAA-visual--------------------------------------------------------------
# OISST_dat |>
#   dplyr::filter(t == "2019-12-01") |>
#   ggplot2::ggplot(aes(x = lon, y = lat)) +
#   ggplot2::geom_tile(aes(fill = temp)) +
#   ggplot2::scale_fill_viridis_c() +
#   ggplot2::coord_quickmap(expand = F) +
#   ggplot2::labs(x = NULL, y = NULL, fill = "SST (°C)") +
#   ggplot2::theme(legend.position = "bottom")

