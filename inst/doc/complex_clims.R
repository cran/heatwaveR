## ----global_options, include = FALSE-------------------------------------
knitr::opts_chunk$set(fig.width = 8, fig.height = 3, fig.align = 'centre',
                      echo = TRUE, warning = FALSE, message = FALSE,
                      eval = TRUE, tidy = FALSE)

## ----load-libs-----------------------------------------------------------
library(tidyverse)
library(ggpubr)
library(heatwaveR)

## ----data-prep-----------------------------------------------------------
# Create tMin time series
tMin <- sst_WA %>% 
  mutate(temp = temp - 1)

# Create tMax time series
tMax <- sst_WA %>% 
  mutate(temp = temp + 1)

## ----clim-calc-----------------------------------------------------------
# The tMax threshold
# The WMO standard climatology period 0f 1981-01-01 to 2010-12-31 should be used where possible.
# Unfortunately, the OISST data, from which these data were drawn, only begin in 1982-01-01
tMax_clim <- ts2clm(tMax, climatologyPeriod = c("1982-01-01", "2011-12-31"), pctile = 90)

# The tMin exceedance
# Note the use here of 'minDuration = 3' and 'maxGap = 1' as the default atmospheric arguments
# The deafult marine arguemnts are 'minDuration = 5' and 'maxGap = 2'
tMin_exc <- exceedance(tMin, threshold = 25, minDuration = 3, maxGap = 1)

# Pull out each data.frame as there own object for easier use
tMin_exc_exceedance <- tMin_exc$exceedance
tMin_exc_threshold <- tMin_exc$threshold

## ----event-detect--------------------------------------------------------
# Note the use here of 'minDuration = 3' and 'maxGap = 1' as the default atmospheric arguments
tMax_event <- detect_event(tMax_clim, minDuration = 3, maxGap = 1)

# Pull out each data.frame as there own object for easier use
tMax_event_event <- tMax_event$event
tMax_event_climatology <- tMax_event$climatology

## ----data-join-----------------------------------------------------------
# Join the climatology outputs of detect_event() and exceedence()
ts_clims <- left_join(tMax_event_climatology, tMin_exc_threshold, by = c("t"))

# Remove all days that did not qualify for exceddence()
ts_clims_filtered <- ts_clims %>%
  filter(exceedance == TRUE)

## ------------------------------------------------------------------------
# Calculate number of days for each event above the 25C threshold
ts_event_duration_thresh <- ts_clims_filtered %>%
  group_by(event_no) %>%
  summarise(event_duration_thresh = n()) %>%
  na.omit()

## ----event-filter1-------------------------------------------------------
# Filter out the events that were not above the static bottom threshold for their entire duration
ts_events_filtered <- left_join(tMax_event_event, ts_event_duration_thresh, by = "event_no") %>%
  na.omit() %>%
  filter(event_duration_thresh == duration)
ts_events_filtered

## ----event-filter2-------------------------------------------------------
ts_events_filtered <- left_join(tMax_event_event, ts_event_duration_thresh, by = "event_no") %>%
  na.omit() %>%
  filter(event_duration_thresh >= duration - 3)
ts_events_filtered

## ----event-filter3-------------------------------------------------------
ts_events_filtered <- left_join(tMax_event_event, ts_event_duration_thresh, by = "event_no") %>%
  na.omit() %>%
  filter(event_duration_thresh >= duration / 4)
ts_events_filtered

## ----visual-prep---------------------------------------------------------
# Create artificial list object similar to detect_event() output
ts_filtered_list <- list(climatology = tMax_event_climatology,
                            event = ts_events_filtered)

## ----event-line1---------------------------------------------------------
# Then run event_line() on it
event_line(ts_filtered_list, start_date = "2010-01-01", end_date = "2012-05-30", spread = 50)

## ----event-line2---------------------------------------------------------
# Or visualise the categories
event_line(ts_filtered_list, start_date = "2010-01-01", end_date = "2012-05-30", 
           spread = 50, category = TRUE)

## ----lolli-plot----------------------------------------------------------
# Or lolli_plot as desired
lolli_plot(ts_filtered_list, event_count = 1)

## ----category------------------------------------------------------------
ts_category <- category(ts_filtered_list, name = "WA")
ts_category

## ------------------------------------------------------------------------
# First we calculate the exceedance as desired
thresh_19 <- exceedance(sst_Med, threshold = 19, minDuration = 10, maxGap = 0)$threshold

# Then we use that output when detecting our events
events_19 <- detect_event(ts2clm(sst_Med, climatologyPeriod = c("1982-01-01", "2011-12-31")), 
                                 threshClim2 = thresh_19$exceedance, minDuration2 = 10, maxGap2 = 0)

## ------------------------------------------------------------------------
# The default output
events_default <- detect_event(ts2clm(sst_Med, climatologyPeriod = c("1982-01-01", "2011-12-31")))

ggarrange(lolli_plot(events_19), lolli_plot(events_default), labels = c("Two thresholds", "One threshold"))

## ------------------------------------------------------------------------
# First we calculate the second threshold
thresh_95 <- detect_event(ts2clm(sst_Med, pctile = 95,
                                 climatologyPeriod = c("1982-01-01", "2011-12-31")), 
                          minDuration = 2, maxGap = 0)$climatology

# Then we use that output when detecting our events
events_95 <- detect_event(ts2clm(sst_Med, climatologyPeriod = c("1982-01-01", "2011-12-31")), 
                          threshClim2 = thresh_95$event, minDuration2 = 2, maxGap2 = 0)

## ------------------------------------------------------------------------
ggarrange(lolli_plot(events_95), lolli_plot(events_default), labels = c("Two thresholds", "One threshold"))

