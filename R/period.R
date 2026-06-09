#' Change Time Series Period (Frequency Aggregation)
#'
#' @description
#' Aggregates an OHLCV time series to a coarser period (daily -> monthly /
#' quarterly / yearly) using \code{tsbox::ts_frequency()} as the backend.
#'
#' By default, an OHLCV-aware aggregation is applied per column based on the
#' suffix after the last underscore in each column name:
#'
#' \itemize{
#'   \item \code{Open}     -> first
#'   \item \code{High}     -> max
#'   \item \code{Low}      -> min
#'   \item \code{Close}    -> last
#'   \item \code{Adjusted} -> last
#'   \item \code{Volume}   -> sum
#' }
#'
#' This matches the financial convention used by \code{xts::to.monthly()},
#' Bloomberg, Yahoo Finance and TradingView.
#'
#' Unrecognized suffixes fall back to \code{"last"} with an informational
#' message. To bypass OHLCV-aware mode and apply a single function to every
#' column, pass \code{aggregate_fn} explicitly (e.g. \code{aggregate_fn = mean}).
#'
#' The target period must be strictly coarser than the input frequency.
#' Requests that would keep the same frequency or disaggregate to a finer
#' frequency fail with an error instead of returning unchanged values.
#'
#' Accepts long \code{data.frame} output from \code{get_stock()}; pivots to
#' a wide \code{xts} internally and returns the result in the same shape as
#' the input.
#'
#' @param data Input object. One of: \code{xts}, \code{tsibble}, \code{zoo},
#'   \code{ts}, or a long \code{data.frame} with columns
#'   \code{Date / Symbol / Column / Value} (the \code{get_stock()} output).
#' @param period character. Target period. One of: \code{"monthly"},
#'   \code{"quarterly"}, \code{"yearly"}. Default: \code{"monthly"}.
#' @param aggregate_fn function or character. Optional override. If supplied,
#'   the same aggregation is applied to every column (any function or string
#'   accepted by \code{tsbox::ts_frequency()}: \code{"sum"}, \code{"mean"},
#'   \code{"first"}, \code{"last"}, or a function). Default: \code{NULL}
#'   (OHLCV-aware mode).
#'
#' @return Same type as the input, aggregated to the requested period.
#'
#' @examples
#' \dontrun{
#' MIS_data    <- get_stock(c("AAPL", "MSFT"),
#'                          start = "2020-01-01",
#'                          columns = c("Close", "Volume"))
#'
#' # OHLCV-aware (Close -> last, Volume -> sum)
#' monthly <- change_period(MIS_data, period = "monthly")
#'
#' # Force mean aggregation on every column
#' monthly_mean <- change_period(MIS_data, period = "monthly", aggregate_fn = mean)
#' }
#'
#' @importFrom tsbox ts_frequency ts_xts ts_tsibble ts_zoo ts_ts
#' @importFrom tidyr pivot_wider pivot_longer separate
#' @importFrom dplyr mutate select arrange
#' @importFrom xts xts
#' @importFrom zoo index
#' @export
change_period <- function(data, period = "monthly", aggregate_fn = NULL) {

  period_map <- c(monthly = "month",
                  quarterly = "quarter", yearly = "year")
  if (!is.character(period) || length(period) != 1 ||
      !(period %in% names(period_map))) {
    stop("'period' must be one of: ",
         paste(names(period_map), collapse = ", "), ".")
  }
  tsbox_period <- period_map[[period]]

  is_long_get_stock <- is.data.frame(data) &&
    all(c("Date", "Symbol", "Column", "Value") %in% colnames(data))

  if (is_long_get_stock) {
    input_kind <- "long_df"
  } else if (inherits(data, "xts")) {
    input_kind <- "xts"
  } else if (inherits(data, "tbl_ts")) {
    input_kind <- "tsibble"
  } else if (inherits(data, "zoo")) {
    input_kind <- "zoo"
  } else if (inherits(data, "ts")) {
    input_kind <- "ts"
  } else {
    stop("'data' must be xts, zoo, tsibble, ts, or a long get_stock ",
         "data.frame (Date/Symbol/Column/Value). Got: ",
         paste(class(data), collapse = "/"), ".")
  }

  # ---- canonicalize to wide xts ----
  if (input_kind == "long_df") {
    wide_df <- data |>
      dplyr::mutate(id = paste(Symbol, Column, sep = "_")) |>
      dplyr::select(Date, id, Value) |>
      dplyr::arrange(Date, id) |>
      tidyr::pivot_wider(names_from = id, values_from = Value)
    dates <- wide_df$Date
    mat   <- as.matrix(as.data.frame(wide_df)[, -1, drop = FALSE])
    x     <- xts::xts(mat, order.by = dates)
  } else if (input_kind == "xts") {
    x <- data
  } else {
    x <- tsbox::ts_xts(data)
  }

  # ---- validate aggregation direction ----
  if (NROW(x) < 2L) {
    stop("Cannot change period: at least two observations are required to ",
         "determine the input frequency.")
  }

  period_rank <- c(
    seconds = 1L, minute = 2L, hourly = 3L, daily = 4L,
    weekly = 5L, monthly = 6L, quarterly = 7L, yearly = 8L
  )
  input_period <- suppressWarnings(xts::periodicity(x)$scale)
  if (is.null(input_period) || !(input_period %in% names(period_rank))) {
    stop("Cannot change period: input frequency could not be determined.")
  }
  if (period_rank[[period]] <= period_rank[[input_period]]) {
    stop("Invalid period conversion from '", input_period, "' to '", period,
         "': target period must be coarser than the input period.")
  }

  # ---- aggregate ----
  if (!is.null(aggregate_fn)) {
    # na.rm = TRUE prevents ts_frequency() from regularizing and padding the
    # series with boundary NAs. Custom functions still need genuine NAs removed.
    if (is.function(aggregate_fn)) {
      user_fn <- aggregate_fn
      aggregate_fn <- function(z) user_fn(z[!is.na(z)])
    }
    result <- tsbox::ts_frequency(
      x, to = tsbox_period, aggregate = aggregate_fn, na.rm = TRUE
    )
    if (!inherits(result, "xts")) {
      result <- tsbox::ts_xts(result)
    }
  } else {
    # Avoid ts_frequency() regularization so already-aggregated inputs are not
    # padded with empty boundary periods before being aggregated again.
    first_non_na <- function(z) {
      z <- z[!is.na(z)]; if (length(z)) z[[1L]] else NA_real_
    }
    last_non_na <- function(z) {
      z <- z[!is.na(z)]; if (length(z)) z[[length(z)]] else NA_real_
    }
    ohlcv_map <- list(
      Open     = first_non_na,
      High     = function(z) {
        z <- z[!is.na(z)]; if (length(z)) max(z) else NA_real_
      },
      Low      = function(z) {
        z <- z[!is.na(z)]; if (length(z)) min(z) else NA_real_
      },
      Close    = last_non_na,
      Adjusted = last_non_na,
      Volume   = function(z) {
        z <- z[!is.na(z)]; if (length(z)) sum(z) else NA_real_
      }
    )
    cols <- colnames(x)
    if (is.null(cols) || length(cols) == 0) {
      stop("Cannot apply OHLCV-aware aggregation: input has no column names. ",
           "Provide 'aggregate_fn' explicitly.")
    }

    agg_list      <- list()
    unknown_cols  <- character()
    for (col in cols) {
      parts  <- strsplit(col, "_")[[1]]
      suffix <- parts[length(parts)]

      if (suffix %in% names(ohlcv_map)) {
        fn <- ohlcv_map[[suffix]]
      } else {
        fn <- last_non_na
        unknown_cols <- c(unknown_cols, col)
      }

      agg_list[[col]] <- tsbox::ts_frequency(
        x[, col], to = tsbox_period, aggregate = fn, na.rm = TRUE
      )
    }

    if (length(unknown_cols) > 0) {
      message("Unrecognized OHLCV suffix in column(s): ",
              paste(unknown_cols, collapse = ", "),
              ". Using 'last' as fallback.")
    }

    result <- do.call(merge, agg_list)
    colnames(result) <- cols
  }

  # ---- convert back to input type ----
  if (input_kind == "long_df") {
    result_df <- as.data.frame(result)
    result_df$Date <- zoo::index(result)
    result_long <- result_df |>
      tidyr::pivot_longer(cols = -Date, names_to = "id", values_to = "Value") |>
      tidyr::separate(id, into = c("Symbol", "Column"),
                      sep = "_(?=[^_]+$)") |>
      dplyr::select(Date, Symbol, Column, Value) |>
      dplyr::arrange(Date, Symbol, Column)
    return(as.data.frame(result_long))
  } else if (input_kind == "xts") {
    return(result)
  } else if (input_kind == "tsibble") {
    return(tsbox::ts_tsibble(result))
  } else if (input_kind == "zoo") {
    return(tsbox::ts_zoo(result))
  } else if (input_kind == "ts") {
    return(tsbox::ts_ts(result))
  }
}
