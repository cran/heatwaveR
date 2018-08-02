## ----global_options, include = FALSE-------------------------------------
knitr::opts_chunk$set(fig.width = 8, fig.height = 3, fig.align = 'centre',
                      echo = TRUE, warning = FALSE, message = FALSE,
                      eval = TRUE, tidy = FALSE)

## ----detect-example1-----------------------------------------------------
# Load libraries
library(heatwaveR)
library(dplyr)

# Detect the events in a time series
ts <- ts2clm(sst_WA, climatologyPeriod = c("1983-01-01", "2012-12-31"))
mhw <- detect_event(ts)

# View just a few metrics
mhw$event %>% 
  dplyr::ungroup() %>%
  dplyr::select(event_no, duration, date_start, date_peak, intensity_mean, intensity_max, intensity_cumulative) %>% 
  dplyr::arrange(-intensity_cumulative) %>% 
  head(5)

## ----fig-example1, echo = TRUE, eval = TRUE------------------------------
event_line(mhw, spread = 100, metric = "intensity_cumulative", start_date = "2010-11-01", end_date = "2011-06-30")

## ----fig-example2, echo = TRUE, eval = TRUE------------------------------
lolli_plot(mhw)

## ----fig-example3, echo = TRUE, eval = TRUE------------------------------
# select the region of the time series of interest
mhw2 <- mhw$clim %>% 
  slice(10580:10690)

ggplot(mhw2, aes(x = t, y = temp, y2 = thresh)) +
  geom_flame() +
  geom_text(aes(x = as.Date("2011-02-26"), y = 25.8, label = "the Destroyer\nof Kelps"))

ggplot(mhw$event, aes(x = date_start, y = intensity_max)) +
  geom_lolli(colour = "salmon", colour_n = "red", n = 3) +
  geom_text(colour = "black", aes(x = as.Date("2006-08-01"), y = 5,
                label = "The marine heatwaves\nTend to be left skewed in a\nGiven time series"))

## ----fig-example4, echo = TRUE, eval = TRUE------------------------------
# It is necessary to give geom_flame() at least one row on either side of the event in order to calculate the polygon corners smoothly
mhw_top <- mhw2 %>% 
  slice(49:110)

ggplot(data = mhw2, aes(x = t)) +
  geom_flame(aes(y = temp, y2 = thresh, fill = "all"), show.legend = T) +
  geom_flame(data = mhw_top, aes(y = temp, y2 = thresh, fill = "top"), show.legend = T) +
  geom_line(aes(y = temp, colour = "temp")) +
  geom_line(aes(y = thresh, colour = "thresh"), size = 1.0) +
  geom_line(aes(y = seas, colour = "seas"), size = 1.2) +
  scale_colour_manual(name = "Line Colour",
                      values = c("temp" = "black", "thresh" =  "forestgreen", "seas" = "grey80")) +
  scale_fill_manual(name = "Event Colour", values = c("all" = "salmon", "top" = "red")) +
  scale_x_date(date_labels = "%b %Y") +
  guides(colour = guide_legend(override.aes = list(fill = NA))) +
  labs(y = expression(paste("Temperature [", degree, "C]")), x = NULL)

## ----fig-example5, echo = TRUE, eval = TRUE------------------------------
ggplot(mhw$event, aes(x = date_start, y = intensity_cumulative)) +
  geom_lolli(colour = "salmon")

## ----detect-example2-----------------------------------------------------
# First calculate the cold-spells
ts_10th <- ts2clm(sst_WA, climatologyPeriod = c("1983-01-01", "2012-12-31"), pctile = 10)
mcs <- detect_event(ts_10th, coldSpells = TRUE)

# Then look at the top few events
mcs$event %>% 
  dplyr::ungroup() %>%
  dplyr::select(event_no, duration, date_start,
                date_peak, intensity_mean, intensity_max, intensity_cumulative) %>%
  dplyr::arrange(intensity_cumulative) %>% 
  head(5)

## ----fig-example6, echo = TRUE, eval = TRUE------------------------------
event_line(mcs, spread = 200, metric = "intensity_cumulative",
           start_date = "1990-01-01", end_date = "1990-08-30")

lolli_plot(mcs)

## ----fig-example7, echo = TRUE, eval = FALSE-----------------------------
#  mcs2 <- mcs$clim %>%
#    slice(2990:3190)
#  
#  # # Note that the plot centres on the polygons, so it may be necessary to manually zoom out a bit
#  ggplot(data = mcs2, aes(x = t)) +
#    geom_flame(aes(y = thresh, y2 = temp), fill = "steelblue3", show.legend = F) +
#    geom_line(aes(y = temp, colour = "temp")) +
#    geom_line(aes(y = thresh, colour = "thresh"), size = 1.0) +
#    geom_line(aes(y = seas, colour = "seas"), size = 1.2) +
#    scale_colour_manual(name = "Line Colour",
#                        values = c("temp" = "black", "thresh" =  "forestgreen", "seas" = "grey80")) +
#    scale_y_continuous(limits = c(18, 23.5)) +
#    scale_x_date(date_labels = "%b %Y") +
#    labs(y = expression(paste("Temperature [", degree, "C]")), x = NULL)
#  
#  ggplot(mcs$event, aes(x = date_start, y = intensity_cumulative)) +
#    geom_lolli(colour = "steelblue3", colour_n = "navy", n = 7) +
#    xlab("Date") +
#    ylab(expression(paste("Cumulative intensity [days x ", degree, "C]")))

