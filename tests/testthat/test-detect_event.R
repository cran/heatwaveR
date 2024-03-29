context("Test detect_event.R")

test_that("detect() returns the correct lists, data.table, and columns", {
  ts <- ts2clm(sst_WA, climatologyPeriod = c("1983-01-01", "2012-12-31"))
  res <- detect_event(ts)
  expect_is(res, "list")
  expect_is(res$climatology, "tbl_df")
  expect_is(res$event, "tbl_df")
  expect_equal(ncol(res$climatology),9)
  expect_equal(ncol(res$event), 22)
})

test_that("all starting error checks flag correctly", {
  ts <- ts2clm(sst_WA, climatologyPeriod = c("1983-01-01", "2012-12-31"))
  expect_error(detect_event(ts, minDuration = "5"),
               "Please ensure that 'minDuration' is a numeric/integer value.")
  expect_error(detect_event(ts, joinAcrossGaps = "TRUE"),
               "Please ensure that 'joinAcrossGaps' is either TRUE or FALSE.")
  expect_error(detect_event(ts, maxGap = "2"),
               "Please ensure that 'maxGap' is a numeric/integer value.")
})

test_that("coldSpells = TRUE returns MCS calculations", {
  ts <- ts2clm(sst_WA, climatologyPeriod = c("1983-01-01", "2012-12-31"), pctile = 10)
  res <- detect_event(ts, coldSpells = TRUE)
  expect_equal(ncol(res$event), 22)
  expect_lt(min(res$event$intensity_max), 0)
})

test_that("joinAcrossGaps = FALSE returns more events", {
  ts <- ts2clm(sst_WA, climatologyPeriod = c("1983-01-01", "2012-12-31"))
  res_join <- detect_event(ts)
  res_misanthrope <- detect_event(ts, joinAcrossGaps = FALSE)
  expect_lt(nrow(res_join$event), nrow(res_misanthrope$event))
})

test_that("events starting/ending before/after the time series dates are dealt with", {
  ts <- ts2clm(sst_WA, climatologyPeriod = c("1983-01-01", "2012-12-31"))
  ts_sub <- ts[885:926, ]
  res <- detect_event(ts_sub)
  res_event <- res$event
  expect_equal(is.na(res_event$rate_onset[1]), TRUE)
  expect_equal(is.na(res_event$rate_decline[3]), TRUE)
})

test_that("detect_event() does not joinAcrossGaps if conditions are not met", {
  ts <- ts2clm(sst_WA, climatologyPeriod = c("1983-01-01", "2012-12-31"))
  res_1 <- detect_event(ts, maxGap = 0)
  res_2 <- detect_event(ts)
  expect_lt(nrow(res_2$event), nrow(res_1$event))
})

test_that("detect_event() utilises the second threshold correctly", {
  ts <- ts2clm(sst_WA, climatologyPeriod = c("1983-01-01", "2012-12-31"))
  ts2 <- exceedance(sst_WA, threshold = 25)
  second_threshold <- ts2$threshold$threshCriterion
  res_1 <- detect_event(ts, threshClim2 = second_threshold)
  res_2 <- detect_event(ts)
  expect_gt(nrow(res_2$event), nrow(res_1$event))
})

test_that("threshClim2 must be logic values", {
  ts <- ts2clm(sst_WA, climatologyPeriod = c("1983-01-01", "2012-12-31"))
  expect_error(detect_event(ts, threshClim2 = "aaa"))
})

test_that("no detected events returns a 1 row NA event dataframe and not an error", {
  sst_WA_flat <- sst_WA
  sst_WA_flat$temp <- 1
  res <- detect_event(ts2clm(sst_WA_flat, climatologyPeriod = c("1983-01-01", "2012-12-31")))
  expect_is(res, "list")
  expect_is(res$climatology, "tbl_df")
  expect_is(res$event, "tbl_df")
  expect_equal(ncol(res$climatology), 9)
  expect_equal(ncol(res$event), 22)
  expect_equal(nrow(res$event), 1)
})

test_that("decimal places are rounded to the fourth place", {
  res <- detect_event(ts2clm(sst_WA, climatologyPeriod = c("1983-01-01", "2012-12-31")))
  expect_equal(nchar(strsplit(as.character(res$climatology$thresh[1]), "\\.")[[1]][2]), 4)
  expect_equal(nchar(strsplit(as.character(res$event$intensity_max[1]), "\\.")[[1]][2]), 4)
})

test_that("protoEvents argument functions correctly", {
  res <- detect_event(ts2clm(sst_WA, climatologyPeriod = c("1983-01-01", "2012-12-31")), protoEvents = T)
  expect_is(res, "data.frame")
})

test_that("roundRes argument functions correctly", {
  ts <- ts2clm(sst_WA, climatologyPeriod = c("1983-01-01", "2012-12-31"))
  res <- detect_event(ts, roundRes = 4)
  expect_equal(res$climatology$seas[1], 21.6080)
  res <- detect_event(ts, roundRes = 0)
  expect_equal(res$event$intensity_max[1], 2)
  res <- detect_event(ts, roundRes = F)
  expect_gt(res$event$rate_decline[1], 0.1782399)
  expect_error(detect_event(ts, roundRes = "Banana"),
               "Please ensure that 'roundRes' is either a numeric value or FALSE.")
})

test_that("only one event with NA for rate_onset or rate_decline returns NA and not error", {
  res_clim <- ts2clm(sst_WA, climatologyPeriod = c("1983-01-01", "2012-12-31"))
  res_onset <- detect_event(res_clim[885:892,])
  res_decline <- detect_event(res_clim[882:890,])
  res_both <- detect_event(res_clim[885:891,])
  expect_equal(res_onset$event$rate_onset, NA)
  expect_equal(res_decline$event$rate_decline, NA)
  expect_equal(res_both$event$rate_onset, NA)
  expect_equal(res_both$event$rate_decline, NA)
})

test_that("built in 'categories' argument works as expected", {
  ts <- ts2clm(sst_WA, climatologyPeriod = c("1983-01-01", "2012-12-31"))
  res_event <- detect_event(ts, categories = T)
  res_list <- detect_event(ts, categories = T, climatology = T)
  res_season <- detect_event(ts, categories = T, season = "peak")
  expect_is(res_event, "data.frame")
  expect_is(res_list, "list")
  expect_equal(res_event$category[1], "I Moderate")
  expect_equal(res_list$climatology$category[889], "I Moderate")
  expect_equal(res_season$season[3], "Winter")
})

test_that("Useful error is returned when incorrect column names exist", {
  ts <- ts2clm(sst_WA, climatologyPeriod = c("1983-01-01", "2012-12-31"))
  colnames(ts) <- c("doy", "banana", "temp", "seas", "thresh")
  expect_error(detect_event(ts),
               "Please ensure that a column named 't' is present in your data.frame or that you have assigned a column to the 'x' argument.")
  colnames(ts) <- c("doy", "t", "banana", "seas", "thresh")
  expect_error(detect_event(ts),
               "Please ensure that a column named 'temp' is present in your data.frame or that you have assigned a column to the 'y' argument.")
  colnames(ts) <- c("doy", "t", "temp", "banana", "thresh")
  expect_error(detect_event(ts),
               "Please ensure that a column named 'seas' is present in your data.frame or that you have assigned a column to the 'seasClim' argument.")
  colnames(ts) <- c("doy", "t", "temp", "seas", "banana")
  expect_error(detect_event(ts),
               "Please ensure that a column named 'thresh' is present in your data.frame or that you have assigned a column to the 'threshClim' argument.")
})
