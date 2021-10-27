## ----global_options, include = FALSE------------------------------------------
knitr::opts_chunk$set(fig.width = 4, fig.align = 'center',
                      echo = TRUE, warning = FALSE, message = FALSE, 
                      eval = FALSE, tidy = FALSE)

## ----load-pkg-----------------------------------------------------------------
#  library(dplyr) # For basic data manipulation
#  library(ncdf4) # For creating NetCDF files
#  library(tidync) # For easily dealing with NetCDF data
#  library(ggplot2) # For visualising data
#  library(doParallel) # For parallel processing

## ----load---------------------------------------------------------------------
#  MHW_res_grid <- readRDS("~/Desktop/MHW_result.Rds")

## ----data-prep----------------------------------------------------------------
#  # Function for creating arrays from data.frames
#  df_acast <- function(df, lon_lat){
#  
#    # Force grid
#    res <- df %>%
#      right_join(lon_lat, by = c("lon", "lat")) %>%
#      arrange(lon, lat)
#  
#    # Convert date values to integers if they are present
#    if(lubridate::is.Date(res[1,4])) res[,4] <- as.integer(res[,4])
#  
#    # Create array
#    res_array <- base::array(res[,4], dim = c(length(unique(lon_lat$lon)), length(unique(lon_lat$lat))))
#    dimnames(res_array) <- list(lon = unique(lon_lat$lon),
#                                lat = unique(lon_lat$lat))
#    return(res_array)
#  }
#  
#  # Wrapper function for last step before data are entered into NetCDF files
#  df_proc <- function(df, col_choice){
#  
#    # Determine the correct array dimensions
#    lon_step <- mean(diff(sort(unique(df$lon))))
#    lat_step <- mean(diff(sort(unique(df$lat))))
#    lon <- seq(min(df$lon), max(df$lon), by = lon_step)
#    lat <- seq(min(df$lat), max(df$lat), by = lat_step)
#  
#    # Create full lon/lat grid
#    lon_lat <- expand.grid(lon = lon, lat = lat) %>%
#      data.frame()
#  
#    # Acast only the desired column
#    dfa <- plyr::daply(df[c("lon", "lat", "event_no", col_choice)],
#                       c("event_no"), df_acast, .parallel = T, lon_lat = lon_lat)
#    return(dfa)
#  }
#  
#  # We must now run this function on each column of data we want to add to the NetCDF file
#  doParallel::registerDoParallel(cores = 7)
#  prep_dur <- df_proc(MHW_res_grid, "duration")
#  prep_max_int <- df_proc(MHW_res_grid, "intensity_max")
#  prep_cum_int <- df_proc(MHW_res_grid, "intensity_cumulative")
#  prep_peak <- df_proc(MHW_res_grid, "date_peak")

## ----nc-shell-----------------------------------------------------------------
#  # Get file attributes
#  lon_step <- mean(diff(sort(unique(MHW_res_grid$lon))))
#  lat_step <- mean(diff(sort(unique(MHW_res_grid$lat))))
#  lon <- seq(min(MHW_res_grid$lon), max(MHW_res_grid$lon), by = lon_step)
#  lat <- seq(min(MHW_res_grid$lat), max(MHW_res_grid$lat), by = lat_step)
#  event_no <- seq(min(MHW_res_grid$event_no), max(MHW_res_grid$event_no), by = 1)
#  tunits <- "days since 1970-01-01"
#  
#  # Length of each attribute
#  nlon <- length(lon)
#  nlat <- length(lat)
#  nen <- length(event_no)

## ----nc-make------------------------------------------------------------------
#  # Path and file name
#    # NB: We are net setting time dimensions here because we are using event_no as our "time" dimension
#  ncpath <- "~/Desktop/"
#  ncname <- "MHW_results"
#  ncfname <- paste0(ncpath, ncname, ".nc")
#  # dname <- "tmp"
#  
#  # Define dimensions
#  londim <- ncdim_def("lon", "degrees_east", lon)
#  latdim <- ncdim_def("lat", "degrees_north", lat)
#  endim <- ncdim_def("event_no", "event_number", event_no)
#  # timedim <- ncdim_def("time", tunits, as.double(time))
#  
#  # Define variables
#  fillvalue <- 1e32
#  def_dur <- ncvar_def(name = "duration", units = "days", dim = list(endim, latdim, londim),
#                       missval = fillvalue, longname = "duration of MHW", prec = "double")
#  def_max_int <- ncvar_def(name = "max_int", units = "deg_c", dim = list(endim, latdim, londim),
#                           missval = fillvalue, longname = "maximum intensity during MHW", prec = "double")
#  def_cum_int <- ncvar_def(name = "cum_int", units = "deg_c days", dim = list(endim, latdim, londim),
#                           missval = fillvalue, longname = "cumulative intensity during MHW", prec = "double")
#  def_peak <- ncvar_def(name = "date_peak", units = tunits, dim = list(endim, latdim, londim),
#                        missval = 0, longname = "date of peak temperature anomaly during MHW", prec = "integer")
#  
#  # Create netCDF file and prepare space for our arrays
#  ncout <- nc_create(ncfname, list(def_peak, def_dur, def_max_int, def_cum_int), force_v4 = TRUE)
#  
#  # Add the actual data
#  ncvar_put(ncout, def_dur, prep_dur)
#  ncvar_put(ncout, def_max_int, prep_max_int)
#  ncvar_put(ncout, def_cum_int, prep_cum_int)
#  ncvar_put(ncout, def_peak, prep_peak)
#  
#  # Put additional attributes into dimension and data variables
#  ncatt_put(ncout, "lon", "axis", "X") #,verbose=FALSE) #,definemode=FALSE)
#  ncatt_put(ncout, "lat", "axis", "Y")
#  ncatt_put(ncout, "event_no", "axis", "event_no")
#  
#  # Add global attributes
#    # These are useful for whomever else may want to use your NetCDF file
#  ncatt_put(ncout, 0, "title", paste0("MHW results from lon: ",
#                                      min(lon)," to ",max(lon),
#                                      " and lat: ",min(lat)," to ",max(lat)))
#  ncatt_put(ncout, 0, "institution", "Your institution here!")
#  ncatt_put(ncout, 0, "source", "NOAA OISST v2.1")
#  ncatt_put(ncout,0, "references", "Banzon et al. (2020) J. Atmos. Oce. Tech. 37:341-349")
#  history <- paste0("Your name here!, ", date())
#  ncatt_put(ncout, 0, "history", history)
#  ncatt_put(ncout, 0, "Conventions", "Hobday et al. (2016)") # Assuming one has used the Hobday definition
#  
#  # Get a summary of the created file:
#  ncout

## ----res-vis------------------------------------------------------------------
#  # Convenience function for comparing files
#  quick_grid <- function(df, var_choice){
#    df %>%
#      filter(event_no == 13) %>%
#      ggplot(aes(x = lon, y = lat)) +
#      geom_raster(aes_string(fill = var_choice)) +
#      coord_cartesian(expand = F) +
#      scale_fill_viridis_c() +
#      theme(legend.position = "bottom")
#  }
#  
#  # Thanks to the tidync package, loading the gridded data is very simple
#  MHW_res_nc <- tidync("~/Desktop/MHW_results.nc") %>%
#    hyper_tibble() %>%
#    mutate(date_peak = as.Date(date_peak, origin = "1970-01-01"))
#  
#  # Plot the duration results
#  quick_grid(MHW_res_grid, "duration")
#  quick_grid(MHW_res_nc, "duration")
#  
#  # Cumulative intensity
#  quick_grid(MHW_res_grid, "intensity_cumulative")
#  quick_grid(MHW_res_nc, "cum_int")

