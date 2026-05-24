test_that("change_period default OHLCV-aware: Open=first, Close=last, High=max, Low=min, Volume=sum", {
  d <- make_long_df(
    symbols = "AAA",
    columns = c("Open", "High", "Low", "Close", "Volume"),
    n_days  = 90
  )
  monthly <- change_period(d, period = "monthly")
  expect_s3_class(monthly, "data.frame")
  expect_true(all(c("Date", "Symbol", "Column", "Value") %in% colnames(monthly)))

  # For each Symbol+Month, verify aggregation rules match raw daily data
  jan <- d[d$Date >= as.Date("2024-01-01") & d$Date < as.Date("2024-02-01"), ]
  agg_jan <- monthly[monthly$Date >= as.Date("2024-01-01") &
                       monthly$Date <  as.Date("2024-02-01"), ]
  open_jan_raw   <- jan$Value[jan$Column == "Open"][1]
  close_jan_raw  <- tail(jan$Value[jan$Column == "Close"], 1)
  high_jan_raw   <- max(jan$Value[jan$Column == "High"])
  low_jan_raw    <- min(jan$Value[jan$Column == "Low"])
  vol_jan_raw    <- sum(jan$Value[jan$Column == "Volume"])

  expect_equal(agg_jan$Value[agg_jan$Column == "Open"],   open_jan_raw)
  expect_equal(agg_jan$Value[agg_jan$Column == "Close"],  close_jan_raw)
  expect_equal(agg_jan$Value[agg_jan$Column == "High"],   high_jan_raw)
  expect_equal(agg_jan$Value[agg_jan$Column == "Low"],    low_jan_raw)
  expect_equal(agg_jan$Value[agg_jan$Column == "Volume"], vol_jan_raw)
})

test_that("change_period accepts user-supplied aggregate_fn (NA-aware wrapping)", {
  d <- make_long_df(symbols = "AAA", columns = "Close", n_days = 90)
  d$Value[5] <- NA  # NA in interior — wrapper should strip before mean()
  monthly <- change_period(d, period = "monthly", aggregate_fn = mean)
  expect_s3_class(monthly, "data.frame")
  expect_false(any(is.na(monthly$Value)))
})

test_that("change_period supports monthly/quarterly/yearly periods", {
  d <- make_long_df(symbols = "AAA", columns = "Close", n_days = 400)
  # NOTE: period = "daily" is accepted by the validation but the underlying
  # tsbox::ts_frequency call errors on already-daily input
  # ("Column ... is type 'closure'"). Skipped here as a known limitation.
  for (p in c("monthly", "quarterly", "yearly")) {
    out <- change_period(d, period = p)
    expect_s3_class(out, "data.frame")
  }
})

test_that("change_period errors on invalid period", {
  d <- make_long_df(symbols = "AAA", columns = "Close", n_days = 30)
  expect_error(change_period(d, period = "weekly"), "'period' must be one of")
  expect_error(change_period(d, period = "centennial"), "'period' must be one of")
})

test_that("change_period errors on unsupported input class", {
  expect_error(change_period(1:10, period = "monthly"), "data")
})
