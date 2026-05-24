test_that("sample_symbols returns deterministic sample with seed", {
  comps <- data.frame(Symbol = LETTERS[1:10], stringsAsFactors = FALSE)
  s1 <- sample_symbols(comps, n = 3, seed = 42)
  s2 <- sample_symbols(comps, n = 3, seed = 42)
  expect_identical(s1, s2)
  expect_length(s1, 3)
  expect_type(s1, "character")
})

test_that("sample_symbols caps n at nrow(components)", {
  comps <- data.frame(Symbol = LETTERS[1:3], stringsAsFactors = FALSE)
  s <- sample_symbols(comps, n = 10, seed = 1)
  expect_length(s, 3)
})

test_that("sample_symbols errors when 'Symbol' column missing", {
  comps <- data.frame(Ticker = LETTERS[1:5], stringsAsFactors = FALSE)
  expect_error(sample_symbols(comps), "Symbol")
})

test_that("get_stock errors on invalid date format", {
  expect_error(
    get_stock("AAPL", start = "not-a-date", end = "2024-12-31"),
    "Invalid date"
  )
})

test_that("get_stock errors on invalid source", {
  expect_error(
    get_stock("AAPL", start = "2024-01-01", end = "2024-01-31",
              source = "spaghetti"),
    "Invalid source"
  )
})

test_that("get_index_components returns a data.frame (network)", {
  skip_on_cran()
  skip_if_offline()
  comps <- get_index_components("DJI")
  expect_s3_class(comps, "data.frame")
  expect_true("Symbol" %in% colnames(comps))
  expect_gt(nrow(comps), 0)
})

test_that("get_stock returns a long data.frame (network)", {
  skip_on_cran()
  skip_if_offline()
  df <- get_stock("AAPL", start = "2024-01-01", end = "2024-01-31",
                  columns = "Close")
  expect_s3_class(df, "data.frame")
  expect_true(all(c("Date", "Symbol", "Column", "Value") %in% colnames(df)))
  expect_gt(nrow(df), 0)
})
