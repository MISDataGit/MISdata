test_that("plot_stock returns a ggplot", {
  d <- make_long_df(symbols = c("AAA", "BBB"), columns = "Close", n_days = 60)
  p <- plot_stock(d)
  expect_s3_class(p, "ggplot")
})

test_that("plot_stock with format = 'long' returns a ggplot", {
  d <- make_long_df(symbols = c("AAA", "BBB"), columns = "Close", n_days = 60)
  p <- plot_stock(d, format = "long")
  expect_s3_class(p, "ggplot")
})

test_that("get_acf returns a data.frame with lag/acf/symbol columns", {
  d <- make_long_df(symbols = "AAA", columns = "Close", n_days = 60)
  tbl <- get_acf(d, symbols = "AAA", lag_max = 20)
  expect_s3_class(tbl, "data.frame")
  expect_true(all(c("lag", "acf", "symbol") %in% colnames(tbl)))
  expect_equal(nrow(tbl), 20)
})

test_that("get_acf with plot = TRUE returns list(table, plot)", {
  d <- make_long_df(symbols = "AAA", columns = "Close", n_days = 60)
  res <- get_acf(d, symbols = "AAA", lag_max = 15, plot = TRUE)
  expect_type(res, "list")
  expect_named(res, c("table", "plot"))
  expect_s3_class(res$plot, "ggplot")
})

test_that("get_pacf returns a data.frame with pacf column", {
  d <- make_long_df(symbols = "AAA", columns = "Close", n_days = 60)
  tbl <- get_pacf(d, symbols = "AAA", lag_max = 20)
  expect_s3_class(tbl, "data.frame")
  expect_true(all(c("lag", "pacf", "symbol") %in% colnames(tbl)))
})

test_that("get_acf errors when symbols is NULL", {
  d <- make_long_df(symbols = "AAA", columns = "Close", n_days = 60)
  expect_error(get_acf(d, symbols = NULL), "symbols")
})

test_that("get_acf warns when more than 5 symbols supplied", {
  d <- make_long_df(symbols = c("S1","S2","S3","S4","S5","S6"),
                    columns = "Close", n_days = 60)
  expect_warning(
    get_acf(d, symbols = c("S1","S2","S3","S4","S5","S6"), lag_max = 10),
    "5 symbols"
  )
})

test_that("plot_seasonal returns a ggplot for a single symbol", {
  # plot_seasonal needs > 2 periods worth of data; with period auto-detection
  # daily series -> 5, so 60 days is enough.
  d <- make_long_df(symbols = "AAA", columns = "Close", n_days = 60)
  p <- plot_seasonal(d, symbol = "AAA", period = 5)
  expect_s3_class(p, "ggplot")
})

test_that("plot_seasonal errors when symbol is NULL", {
  d <- make_long_df(symbols = "AAA", columns = "Close", n_days = 60)
  expect_error(plot_seasonal(d, symbol = NULL), "symbol")
})

test_that("plot_seasonal errors when more than one symbol supplied", {
  d <- make_long_df(symbols = c("AAA", "BBB"), columns = "Close", n_days = 60)
  expect_error(plot_seasonal(d, symbol = c("AAA", "BBB")), "single value")
})
