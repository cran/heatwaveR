## ----global_options, include = FALSE-------------------------------------
knitr::opts_chunk$set(fig.width = 8, fig.height = 3, fig.align = 'centre',
                      echo = TRUE, warning = FALSE, message = FALSE,
                      eval = FALSE, tidy = FALSE)

## ----setup---------------------------------------------------------------
#  # The two packages we will need
#    # NB: The packages only need to be installed from GitHub once
#  # devtools::install_github("tidyverse/tidyverse")
#  # devtools::install_github("ropensci/rerddap")
#  
#  # Load the packages once they have been downloaded and installed
#  library(tidyverse)
#  library(rerddap)
#  
#  # The information for the NOAA OISST data
#  rerddap::info(datasetid = "ncdc_oisst_v2_avhrr_by_time_zlev_lat_lon", url = "https://www.ncei.noaa.gov/erddap/")

## ----download-func-------------------------------------------------------
#  # This function expects the user to provide it with a start and end date
#  # It then downloads and prepares the data
#  OISST_sub <- function(time_df){
#    oisst_res <- griddap(x = "ncdc_oisst_v2_avhrr_by_time_zlev_lat_lon",
#                         url = "https://www.ncei.noaa.gov/erddap/",
#                         time = c(time_df$start, time_df$end),
#                         depth = c(0, 0),
#                         latitude = c(-40, -35),
#                         longitude = c(15, 21),
#                         fields = "sst")$data %>%
#      mutate(time = as.Date(str_remove(time, "T00:00:00Z"))) %>%
#      dplyr::rename(t = time, temp = sst) %>%
#      select(lon, lat, t, temp) %>%
#      na.omit()
#  }

## ----year-index----------------------------------------------------------
#  # Date download range by start and end dates per year
#  dl_years <- data.frame(date_index = 1:5,
#                         start = as.Date(c("1982-01-01", "1990-01-01",
#                                           "1998-01-01", "2006-01-01", "2014-01-01")),
#                         end = as.Date(c("1989-12-31", "1997-12-31",
#                                         "2005-12-31", "2013-12-31", "2018-12-31")))

## ----download-data-------------------------------------------------------
#  # Download all of the data with one nested request
#  # The time this takes will vary greatly based on connection speed
#  system.time(
#    OISST_data <- dl_years %>%
#      group_by(date_index) %>%
#      group_modify(~OISST_sub(.x)) %>%
#      ungroup() %>%
#      select(lon, lat, t, temp)
#  ) # 921 seconds, ~184 seconds per batch

## ----prep-data-----------------------------------------------------------
#  # Save the data as an .Rda file as it has a much better compression rate than .RData
#  saveRDS(OISST_data, file = "~/Desktop/OISST_vignette.Rda")

