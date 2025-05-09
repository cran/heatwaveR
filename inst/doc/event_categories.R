## ----global_options, include = FALSE------------------------------------------
knitr::opts_chunk$set(fig.width = 8, fig.height = 3, fig.align = 'centre',
                      echo = TRUE, warning = FALSE, message = FALSE,
                      eval = TRUE, tidy = FALSE)

## ----category-example-1-------------------------------------------------------
# Load libraries
library(dplyr)
library(tidyr)
library(ggplot2)
library(heatwaveR)

# Calculate events
ts <- ts2clm(sst_WA, climatologyPeriod = c("1982-01-01", "2011-12-31"))
MHW <- detect_event(ts) 
MHW_cat <- category(MHW, S = TRUE, name = "WA")

# Look at the top few events
tail(MHW_cat)

## ----category-example-2-------------------------------------------------------
res_Med <- detect_event(ts2clm(sst_Med, climatologyPeriod = c("1982-01-01", "2011-12-31")))
res_Med_cat <- category(res_Med, S = FALSE, name = "Med")
tail(res_Med_cat)

## -----------------------------------------------------------------------------
# Add lon/lat to the three default time series
ts_WA <- sst_WA |> mutate(site = "WA", lon = 112.625, lat = -29.375)
ts_NW_Atl <- sst_NW_Atl |> mutate(site = "NW_Atl", lon = -66.875, lat = 43.125)
ts_Med <- sst_Med |> mutate(site = "Med", lon = 9.125, lat = 43.625)
ts_ALL <- rbind(ts_WA, ts_NW_Atl, ts_Med)

# Calculate MHW categories by site
MHW_cat_ALL <- ts_ALL |> 
  group_by(site) |> 
  group_modify(~ {
    .x |> 
      ts2clm(climatologyPeriod = c("1982-01-01", "2011-12-31")) |> 
      detect_event() |> 
      category(season = "peak", lat_col = TRUE)
    }) |> 
  # Correct event names by site
  mutate(event_name = case_when(!is.na(event_name) ~ stringr::str_replace(event_name, "Event", site)))

# View results
MHW_cat_ALL |> 
  arrange(-duration) |> 
  filter(!is.na(event_name)) |> 
  group_by(site) |> 
  group_modify(~ head(.x, 2L)) |> 
  dplyr::select(site:category, duration, season)

## ----fig-example-1, echo = TRUE, eval = TRUE----------------------------------
event_line(MHW, spread = 100, start_date = "2010-11-01", end_date = "2011-06-30", category = TRUE)

## ----fig-example-2, echo = TRUE, eval = TRUE----------------------------------
# Create category breaks and select slice of data.frame
clim_cat <- MHW$clim %>%
  dplyr::mutate(diff = thresh - seas,
                thresh_2x = thresh + diff,
                thresh_3x = thresh_2x + diff,
                thresh_4x = thresh_3x + diff) %>% 
  dplyr::slice(10580:10690)

# Set line colours
lineColCat <- c(
  "Temperature" = "black",
  "Climatology" = "gray20",
  "Threshold" = "darkgreen",
  "2x Threshold" = "darkgreen",
  "3x Threshold" = "darkgreen",
  "4x Threshold" = "darkgreen"
  )

# Set category fill colours
fillColCat <- c(
  "Moderate" = "#ffc866",
  "Strong" = "#ff6900",
  "Severe" = "#9e0000",
  "Extreme" = "#2d0000"
  )

ggplot(data = clim_cat, aes(x = t, y = temp)) +
  geom_flame(aes(y2 = thresh, fill = "Moderate")) +
  geom_flame(aes(y2 = thresh_2x, fill = "Strong")) +
  geom_flame(aes(y2 = thresh_3x, fill = "Severe")) +
  geom_flame(aes(y2 = thresh_4x, fill = "Extreme")) +
  geom_line(aes(y = thresh_2x, col = "2x Threshold"), size = 0.7, linetype = "dashed") +
  geom_line(aes(y = thresh_3x, col = "3x Threshold"), size = 0.7, linetype = "dotdash") +
  geom_line(aes(y = thresh_4x, col = "4x Threshold"), size = 0.7, linetype = "dotted") +
  geom_line(aes(y = seas, col = "Climatology"), size = 0.7) +
  geom_line(aes(y = thresh, col = "Threshold"), size = 0.7) +
  geom_line(aes(y = temp, col = "Temperature"), size = 0.6) +
  scale_colour_manual(name = NULL, values = lineColCat,
                      breaks = c("Temperature", "Climatology", "Threshold",
                                 "2x Threshold", "3x Threshold", "4x Threshold")) +
  scale_fill_manual(name = NULL, values = fillColCat, guide = FALSE) +
  scale_x_date(date_labels = "%b %Y") +
  guides(colour = guide_legend(override.aes = list(linetype = c("solid", "solid", "solid",
                                                                "dashed", "dotdash", "dotted"),
                                                   size = c(0.6, 0.7, 0.7, 0.7, 0.7, 0.7)))) +
  labs(y = "Temperature [°C]", x = NULL)

## ----category-example-3-------------------------------------------------------
# Calculate events
ts_MCS <- ts2clm(sst_WA, climatologyPeriod = c("1982-01-01", "2011-12-31"), pctile = 10)
MCS <- detect_event(ts_MCS, coldSpells = T)
MCS_cat <- category(MCS, S = TRUE, name = "WA")

# Look at the top few events
tail(MCS_cat)

## ----fig-example-3, echo = TRUE, eval = TRUE----------------------------------
event_line(MCS, spread = 100, start_date = "1989-11-01", end_date = "1990-06-30", category = TRUE)

## ----fig-example-4, echo = TRUE, eval = TRUE----------------------------------
# Create category breaks and select slice of data.frame
MCS_clim_cat <- MCS$clim %>%
  dplyr::mutate(diff = thresh - seas,
                thresh_2x = thresh + diff,
                thresh_3x = thresh_2x + diff,
                thresh_4x = thresh_3x + diff) %>% 
  dplyr::slice(2910:3150)

# Set line colours
lineColCat <- c(
  "Temperature" = "black",
  "Climatology" = "grey40",
  "Threshold" = "darkorchid",
  "2x Threshold" = "darkorchid",
  "3x Threshold" = "darkorchid",
  "4x Threshold" = "darkorchid"
  )

# Set category fill colours
fillColCat <- c(
  "Moderate" = "#C7ECF2",
  "Strong" = "#85B7CC",
  "Severe" = "#4A6A94",
  "Extreme" = "#111433"
  )

# Create plot
ggplot(data = MCS_clim_cat, aes(x = t, y = temp)) +
  geom_flame(aes(y = thresh, y2 = temp, fill = "Moderate")) +
  geom_flame(aes(y = thresh_2x, y2 = temp, fill = "Strong")) +
  geom_flame(aes(y = thresh_3x, y2 = temp, fill = "Severe")) +
  geom_flame(aes(y = thresh_4x, y2 = temp, fill = "Extreme")) +
  geom_line(aes(y = thresh_2x, col = "2x Threshold"), size = 0.7, linetype = "dashed") +
  geom_line(aes(y = thresh_3x, col = "3x Threshold"), size = 0.7, linetype = "dotdash") +
  geom_line(aes(y = thresh_4x, col = "4x Threshold"), size = 0.7, linetype = "dotted") +
  geom_line(aes(y = seas, col = "Climatology"), size = 0.7) +
  geom_line(aes(y = thresh, col = "Threshold"), size = 0.7) +
  geom_line(aes(y = temp, col = "Temperature"), size = 0.6) +
  scale_colour_manual(name = NULL, values = lineColCat,
                      breaks = c("Temperature", "Climatology", "Threshold",
                                 "2x Threshold", "3x Threshold", "4x Threshold")) +
  scale_fill_manual(name = NULL, values = fillColCat, guide = FALSE) +
  scale_x_date(date_labels = "%b %Y") +
  guides(colour = guide_legend(override.aes = list(linetype = c("solid", "solid", "solid",
                                                                "dashed", "dotdash", "dotted"),
                                                   size = c(0.6, 0.7, 0.7, 0.7, 0.7, 0.7)))) +
  labs(y = "Temperature [°C]", x = NULL)

## ----comp-fig-----------------------------------------------------------------
# The MCS colour palette
MCS_colours <- c(
  "Moderate" = "#C7ECF2",
  "Strong" = "#85B7CC",
  "Severe" = "#4A6A94",
  "Extreme" = "#111433"
)

# The MHW colour palette
MHW_colours <- c(
  "Moderate" = "#ffc866",
  "Strong" = "#ff6900",
  "Severe" = "#9e0000",
  "Extreme" = "#2d0000"
)

# Create the colour palette for plotting by itself
colour_palette <- data.frame(category = factor(c("I Moderate", "II Strong", "III Severe", "IV Extreme"),
                                               levels = c("I Moderate", "II Strong", "III Severe", "IV Extreme")),
                             MHW = c(MHW_colours[1], MHW_colours[2], MHW_colours[3], MHW_colours[4]),
                             MCS = c(MCS_colours[1], MCS_colours[2], MCS_colours[3], MCS_colours[4])) %>% 
  pivot_longer(cols = c(MHW, MCS), names_to = "event", values_to = "colour")

# Show the palettes side-by-side
ggplot(data = colour_palette, aes(x = category, y = event)) +
  geom_tile(fill = colour_palette$colour) +
  coord_cartesian(expand = F) +
  labs(x = NULL, y = NULL)

