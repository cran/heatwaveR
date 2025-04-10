## ----global_options, include = FALSE------------------------------------------
knitr::opts_chunk$set(fig.width = 4, fig.align = 'center',
                      echo = FALSE, warning = FALSE, 
                      message = FALSE, tidy = FALSE)

## ----netCDF2csv, echo = TRUE, eval = FALSE------------------------------------
# # Load packages -----------------------------------------------------------
# 
# library(tidyverse)
# library(tidync)
# library(data.table) # for the fast fwrite() function
# library(heatwaveR)
# library(doParallel)
# registerDoParallel(cores = 14) # not using all 16
# 
# # Define paths ------------------------------------------------------------
# 
# datadir <- "/Volumes/OceanData"
# oisst_file_dir <- paste0(datadir, "/test_files")
# nc_file <- paste0(oisst_file_dir, "/OISST_combined.nc")
# MHW_dir <- datadir
# 
# # Define various functions ------------------------------------------------
# 
# # A load and slice function for the combined netCDF
# OISST_load <- function(file_name, lon1, lon2) {
#   OISST_dat <- tidync(file_name) %>%
#     hyper_filter(lon = between(lon, lon1, lon2)) %>%
#     hyper_tibble(select_var = "sst", force = TRUE, drop = TRUE) %>%
#     select(-zlev) %>%
#     dplyr::rename(t = time, temp = sst) %>%
#     mutate(t = as.Date(t, origin = "1978-01-01"))
#   return(OISST_dat)
#   rm(OISST_dat)
# }
# 
# # Rob's MHW detect function
# event_only <- function(df) {
#   # first calculate the climatologies
#   clim <- ts2clm(data = df, climatologyPeriod = c("1991-01-01", "2020-12-31"))
#   # then the events
#   event <- detect_event(data = clim)
#   rm(clim)
#   # return only the event metric dataframe of results
#   return(event$event)
#   rm(event)
# }

## ----detectMHWs, echo = TRUE, eval = FALSE------------------------------------
# # Execute the code --------------------------------------------------------
# 
# # Define the slices
# # 10° longitude slices seem to work fine on
# # my MacBook Pro with 64Gb RAM and 16 cores
# slice_df <- tibble(lon1 = seq(0, 350, 10),
#                    lon2 = seq(10, 360, 10))
# 
# system.time(
#   # extract slices sequentially
#   for (i in 1:nrow(slice_df)) {
#     cat(noquote(paste("Processing slice", i, "of", nrow(slice_df),
#                       "-->", slice_df$lon1[i], "to", slice_df$lon2[i], "°E\n")))
#     cat(noquote("  > 1. loading and slicing NetCDF\n"))
#     sst <- OISST_load(nc_file, lon1 = slice_df$lon1[i], lon2 = slice_df$lon2[i])
#     # process each slice in parallel
#     cat(noquote("  > 2. detecting marine heatwaves\n"))
#     MHW <- plyr::ddply(.data = sst, .variables = c("lon", "lat"),
#                        .fun = event_only, .parallel = TRUE)
#     rm(sst)
#     # save results to disk
#     cat(noquote("  > 3. saving events to csv\n"))
#     fwrite(MHW, file = paste0(datadir, "/MHW_slice_", i, "_",
#                               slice_df$lon1[i], "-", slice_df$lon2[i], ".csv"))
#     rm(MHW)
#     cat(noquote("SLICE DONE!\n"))
#     cat(sep="\n\n")
#   }
# )

