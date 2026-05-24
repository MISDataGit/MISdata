test_that("clean_stock with na_method = 'trim' drops leading/trailing NA", {
  d <- make_long_df(symbols = "AAA", columns = "Close", n_days = 10)
  d$Value[1:2]  <- NA  # leading
  d$Value[9:10] <- NA  # trailing
  d$Value[5]    <- NA  # interior — should remain
  cleaned <- clean_stock(d, na_method = "trim")
  expect_equal(nrow(cleaned), 6)
  expect_true(any(is.na(cleaned$Value)))
})

test_that("clean_stock with na_method = 'approx' fills interior NA", {
  d <- make_long_df(symbols = "AAA", columns = "Close", n_days = 10)
  d$Value[5] <- NA
  cleaned <- clean_stock(d, na_method = "approx")
  expect_false(any(is.na(cleaned$Value)))
  expect_equal(nrow(cleaned), 10)
})

test_that("clean_stock applies trim and approx together", {
  d <- make_long_df(symbols = "AAA", columns = "Close", n_days = 10)
  d$Value[1]    <- NA
  d$Value[5]    <- NA
  d$Value[10]   <- NA
  cleaned <- clean_stock(d, na_method = c("trim", "approx"))
  expect_equal(nrow(cleaned), 8)
  expect_false(any(is.na(cleaned$Value)))
})

test_that("clean_stock is group-wise on Symbol/Column", {
  d <- make_long_df(symbols = c("AAA", "BBB"), columns = "Close", n_days = 10)
  d$Value[d$Symbol == "AAA"][1:2] <- NA  # only AAA has leading NAs
  cleaned <- clean_stock(d, na_method = "trim")
  expect_equal(sum(cleaned$Symbol == "AAA"), 8)
  expect_equal(sum(cleaned$Symbol == "BBB"), 10)
})

test_that("clean_stock errors on non-data.frame input", {
  expect_error(clean_stock(1:10), "data.frame")
})

test_that("clean_stock errors on missing required columns", {
  bad <- data.frame(Date = Sys.Date(), Foo = 1)
  expect_error(clean_stock(bad), "Missing columns")
})

test_that("clean_stock errors on empty data.frame", {
  empty <- data.frame(Date = as.Date(character()),
                      Symbol = character(),
                      Column = character(),
                      Value  = numeric())
  expect_error(clean_stock(empty), "empty")
})
