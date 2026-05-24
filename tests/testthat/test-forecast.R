test_that("forecast_stock returns list(model, plot, forecast) in that order", {
  d <- make_long_df(symbols = "AAA", columns = "Close", n_days = 120)
  res <- suppressMessages(
    forecast_stock(d, symbol = "AAA", horizon = 10, period = 5)
  )
  expect_type(res, "list")
  expect_named(res, c("model", "plot", "forecast"))
  expect_s3_class(res$model, "Arima")
  expect_s3_class(res$plot, "ggplot")
  expect_s3_class(res$forecast, "forecast")
})

test_that("forecast_stock errors when horizon is 0 or negative", {
  d <- make_long_df(symbols = "AAA", columns = "Close", n_days = 120)
  expect_error(
    suppressMessages(forecast_stock(d, symbol = "AAA", horizon = 0)),
    "horizon"
  )
  expect_error(
    suppressMessages(forecast_stock(d, symbol = "AAA", horizon = -5)),
    "horizon"
  )
})

test_that("forecast_stock errors when symbol is NULL", {
  d <- make_long_df(symbols = "AAA", columns = "Close", n_days = 120)
  expect_error(
    suppressMessages(forecast_stock(d, symbol = NULL, horizon = 10)),
    "symbol"
  )
})

test_that("forecast_stock errors when ci_levels are all invalid", {
  d <- make_long_df(symbols = "AAA", columns = "Close", n_days = 120)
  expect_error(
    suppressMessages(
      forecast_stock(d, symbol = "AAA", horizon = 10, ci_levels = c(-10, 150))
    ),
    "ci_levels"
  )
})

test_that("forecast_stock warns when some ci_levels are invalid", {
  d <- make_long_df(symbols = "AAA", columns = "Close", n_days = 120)
  expect_warning(
    suppressMessages(
      forecast_stock(d, symbol = "AAA", horizon = 10,
                     ci_levels = c(80, 150), period = 5)
    )
  )
})

test_that("forecast_stock produces the requested number of forecast steps", {
  d <- make_long_df(symbols = "AAA", columns = "Close", n_days = 120)
  res <- suppressMessages(
    forecast_stock(d, symbol = "AAA", horizon = 7, period = 5)
  )
  expect_length(as.numeric(res$forecast$mean), 7)
})
