test_that("convert_stock to xts returns xts with Symbol_Column names", {
  d <- make_long_df(symbols = "AAA", columns = c("Close", "Volume"), n_days = 30)
  x <- convert_stock(d, to = "xts")
  expect_s3_class(x, "xts")
  expect_true(all(c("AAA_Close", "AAA_Volume") %in% colnames(x)))
})

test_that("convert_stock to tsibble returns tbl_ts", {
  d <- make_long_df(symbols = "AAA", columns = "Close", n_days = 30)
  out <- convert_stock(d, to = "tsibble")
  expect_s3_class(out, "tbl_ts")
})

test_that("convert_stock to zoo returns zoo", {
  d <- make_long_df(symbols = "AAA", columns = "Close", n_days = 30)
  out <- convert_stock(d, to = "zoo")
  expect_s3_class(out, "zoo")
})

test_that("convert_stock to ts returns ts", {
  d <- make_long_df(symbols = "AAA", columns = "Close", n_days = 30)
  out <- convert_stock(d, to = "ts")
  expect_s3_class(out, "ts")
})

test_that("convert_stock to df returns data.frame", {
  d <- make_long_df(symbols = "AAA", columns = "Close", n_days = 30)
  out <- convert_stock(d, to = "df")
  expect_s3_class(out, "data.frame")
})

test_that("convert_stock errors on invalid 'to'", {
  d <- make_long_df(symbols = "AAA", columns = "Close", n_days = 30)
  expect_error(convert_stock(d, to = "spaghetti"), "'to' must be one of")
})

test_that("convert_stock errors on invalid 'format'", {
  d <- make_long_df(symbols = "AAA", columns = "Close", n_days = 30)
  expect_error(convert_stock(d, to = "tsibble", format = "diagonal"), "format")
})
