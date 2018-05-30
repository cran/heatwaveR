## ----global_options, include = FALSE-------------------------------------
knitr::opts_chunk$set(fig.width = 4, fig.align = 'center',
                      echo = FALSE, warning = FALSE, 
                      message = FALSE, tidy = FALSE)

## ----netCDF2csv, echo = TRUE, eval = FALSE-------------------------------
#  # generic_netCDF2csv.R
#  
#  
#  # NOTES ON USING THIS SCRIPT ----------------------------------------------
#  
#  # 1. The Reynolds OISST v2 data processed by this script can be retrieved from:
#  # https://podaac.jpl.nasa.gov/dataset/AVHRR_OI-NCEI-L4-GLOB-v2.0
#  # 2. Subsetting and the selection of the time steps to be done via the python
#  # script `subset_dataset.py`.
#  # 3. This R script requires the already subsetted netCDFs to reside inside a
#  # directory whose path is specified by `nc.dir`, below.
#  # 4. The .csv files produced will be placed inside of the directory named by
#  # `csv.dir` (make sure this directory already exists).
#  # 5. The dates that will be included with the final .csv file will be extracted
#  # directly from the names of the daily netCDF files; please, therefore, make
#  # sure to never change them by manually editing them.
#  # 6. The base name of the new .csv file will be partly based on the name of the
#  # input netCDF files, with the start and end dates appended at the end. These
#  # things are hard coded into the script below.
#  # 7. I am sure I have missed some things, or that some things may break somehow;
#  # please let me know if this happens and I shall fix it.
#  # 8. This file may take a while to run (10s of minutes to hours, depending on
#  # the amount of data processed); please be patient while it does its thing.
#  
#  # Author: AJ Smit
#  # Date: 27 April 2018
#  # e-mail: ajsmit@uwc.ac.za
#  
#  
#  # CAUTION -----------------------------------------------------------------
#  
#  # This function will append data to the end of an existing file that had been
#  # previously produced by this script. This will result in duplicate data. If you
#  # need to rerun the script for some reason, please make sure to delete the file
#  # created as the result of the previous run from the `csv.dir`.
#  
#  
#  # LOAD LIBRARIES ----------------------------------------------------------
#  
#  library(ncdf4) # library for processing netCDFs
#  library(data.table) # for fast .csv write function, `fwrite()`
#  library(tidyverse) # misc. data processing conveniences
#  library(reshape2) # for making a long data format
#  library(plyr) # for `llply()`
#  library(lubridate) # for working with dates
#  library(stringr) # for working with strings
#  library(doMC); doMC::registerDoMC(cores = 4) # for multicore spead-ups
#  
#  
#  # SPECIFY FILE PATHS ------------------------------------------------------
#  
#  # Setup OISST netCDF data path and csv file output directory
#  nc.dir <- "/Users/ajsmit/spatial/test/netCDF"
#  csv.dir <- "/Users/ajsmit/spatial/test/csv"
#  
#  
#  # PARSE FILE INFO (not used directly) -------------------------------------
#  
#  # Use to determine the start/end points of the `name.stem` (see code below)
#  #          1         2         3         4         5         6         7
#  # 123456789012345678901234567890123456789012345678901234567890123456789012345
#  # 20091231120000-NCEI-L4_GHRSST-SSTblend-AVHRR_OI-GLOB-v02.0-fv02.0_subset.nc
#  
#  
#  # OISST netCDF READ FUNCTION ----------------------------------------------
#  
#  # Function to extract the dims and data from OISST netCDFs
#  ncRead <- function(nc.dir = nc.dir, csv.dir = csv.dir) {
#    nc.list <- list.files(path = nc.dir, pattern = "*.nc", full.names = TRUE, include.dirs = TRUE)
#    strt.date <- str_sub(basename(nc.list[1]), start = 1, end = 8)
#    end.date <- str_sub(basename(nc.list[length(nc.list)]), start = 1, end = 8)
#  
#    ncFun <- function(nc.file = nc.file, csv.dir = csv.dir) {
#      nc <- nc_open(nc.file)
#      name.stem <- substr(basename(nc.file), 16, 72)
#      date.stamp <- substr(basename(nc.file), 1, 8)
#      sst <- round(ncvar_get(nc, varid = "analysed_sst"), 4)
#      dimnames(sst) <- list(lon = nc$dim$lon$vals,
#                            lat = nc$dim$lat$vals)
#      nc_close(nc)
#      sst <-
#        as.data.frame(melt(sst, value.name = "temp"), row.names = NULL) %>%
#        mutate(t = ymd(date.stamp)) %>%
#        na.omit()
#      # Use data.table's `fwrite()` because it is much faster than anything else
#      # for large csv files
#      fwrite(sst,
#             file = paste0(csv.dir, "/", name.stem, "-", strt.date, "-", end.date, ".csv"),
#             append = TRUE, col.names = FALSE)
#      rm(sst)
#    }
#    # `parallel = FALSE` allows for better error checking should something go wrong;
#    # there's also a helpful progress bar that becomes available
#    llply(nc.list, ncFun, csv.dir = csv.dir, .parallel = FALSE)
#  }

## ----run-netCDF2csv, echo = TRUE, eval = FALSE---------------------------
#  # RUN THE FUNCTION --------------------------------------------------------
#  
#  # If everything works according to plan, all that's required is to execute
#  # this line as is after specifying `nc.dir` and `csv.dir` paths, above
#  system.time(ncRead(nc.dir, csv.dir))

## ----OISST_detect, echo = TRUE, eval = FALSE-----------------------------
#  # SET-UP SOME THINGS ------------------------------------------------------
#  
#  csv.dir <- "/Users/ajsmit/spatial/test/csv"
#  
#  # Specify the name of the file created by `system.time(ncRead(nc.dir, csv.dir))` in the
#  # `csv.dir` that was named above
#  csv.file <- "NCEI-L4_GHRSST-SSTblend-AVHRR_OI-GLOB-v02.0-fv02.0_subset-19810901-20171014.csv"
#  
#  # Read in the data in the combined .csv file and assign column names;
#  # again using the data.table way (`fread()`), which is FAST...
#  sst.dat <- fread(paste0(csv.dir, "/", csv.file))
#  colnames(sst.dat) <- c("lon", "lat", "temp", "t")
#  
#  
#  # A FAST DATE FUNCTION ----------------------------------------------------
#  
#  # This speeds the creation of the date vector up immensely
#  fastDate <- function(x, tz = NULL) {
#    require(fasttime)
#    as.Date(fastPOSIXct(x, tz = tz))
#  }
#  
#  
#  # APPLY FAST DATE FUNCTION ------------------------------------------------
#  
#  sst.dat$t <- fastDate(sst.dat$t)
#  
#  
#  # DETECT FUNCTION FOR GRIDDED DATA ----------------------------------------
#  
#  # The `clim` switch selects either climatology or events as output;
#  # please provide your own date range in here
#  OISST_detect <- function(dat = data, doy = doy, x = t, y = temp,
#                           warm = TRUE, clim = FALSE,
#                           climatology_start = "1983-01-01",
#                           climatology_end = "2012-12-31") {
#    require(RmarineHeatWaves)
#    dat <- dat[,3:4] # hard-code cols with temp and date
#    whole <- make_whole(dat)
#    if (warm) {
#      out <- detect(whole, min_duration = 5, max_gap = 2, cold_spells = FALSE,
#                    climatology_start = climatology_start,
#                    climatology_end = climatology_end)
#    } else {
#      out <- detect(whole, min_duration = 5, max_gap = 2, cold_spells = TRUE,
#                    climatology_start = climatology_start,
#                    climatology_end = climatology_end)
#    }
#    if (clim) {
#      clim <- out$clim
#      return(clim)
#    } else {
#      event <- out$event
#      return(event)
#    }
#  }
#  
#  
#  # DETECT EVENTS! ----------------------------------------------------------
#  
#  events <- as_tibble(ddply(sst.dat, .(lon, lat), OISST_detect,
#                            warm = TRUE, clim = FALSE,
#                            .parallel = FALSE, .progress = "text"))
#  
#  
#  # SAVE AND CLEAN UP -------------------------------------------------------
#  event.dir <- "/Users/ajsmit/spatial/test/events"
#  
#  # Reuse the existing name; this ensures traceability of the files back to
#  # the input sources
#  event.file <- gsub(pattern = "\\.csv$", "", csv.file)
#  
#  # Write the file out to disk
#  fwrite(events, file = paste0("event.dir", "/", event.file, "-events.csv"))
#  remove(events); remove(sst.dat)

