# ==============================================================================
# forecast.R - ARIMA-based Forecasting
#
# Public function
# ---------------
#   forecast_stock : Fits forecast::auto.arima() on a single symbol's series
#                    and returns model + forecast object + ggplot in a list.
#
# Accepted input types:
#   - long data.frame  : Date / Symbol / Column / Value (get_stock() output)
#   - wide xts         : convert_stock(to = "xts") output
# ==============================================================================


# ----------------------------------------------------------------------------
# Private helpers - kept here (rather than sourced from eda.R) so this file
# is self-contained when sourced. Same code as eda.R; duplication is
# intentional (kept short on purpose).
# ----------------------------------------------------------------------------

#' @keywords internal
#' @noRd
.to_long_df <- function(data) {

  if (is.data.frame(data) &&
      all(c("Date", "Symbol", "Column", "Value") %in% colnames(data))) {
    return(as.data.frame(data))
  }

  if (inherits(data, "xts")) {
    wide_df <- as.data.frame(data)
    wide_df$Date <- zoo::index(data)
    rownames(wide_df) <- NULL
    wide_df <- wide_df[, c("Date", setdiff(colnames(wide_df), "Date"))]

    long_df <- wide_df |>
      tidyr::pivot_longer(cols = -Date, names_to = "id", values_to = "Value") |>
      tidyr::separate(id, into = c("Symbol", "Column"),
                      sep = "_(?=[^_]+$)", fill = "right") |>
      dplyr::arrange(Date, Symbol, Column) |>
      as.data.frame()

    long_df$Column[is.na(long_df$Column)] <- "value"
    return(long_df)
  }

  stop("'data' must be a long format data.frame ",
       "(columns: Date, Symbol, Column, Value) or a wide xts object. ",
       "Got: ", paste(class(data), collapse = "/"), ".")
}


#' @keywords internal
#' @noRd
.filter_dates <- function(data, start = NULL, end = NULL) {

  parse_one <- function(d, name) {
    parsed <- suppressWarnings(
      anytime::anydate(stringr::str_trim(as.character(d)))
    )
    if (is.na(parsed)) {
      stop("Invalid '", name, "' date. Use 'YYYY-MM-DD'.")
    }
    parsed
  }

  s <- if (!is.null(start)) parse_one(start, "start") else NULL
  e <- if (!is.null(end))   parse_one(end,   "end")   else NULL

  if (!is.null(s) && !is.null(e) && s > e) {
    stop("'start' (", s, ") must be <= 'end' (", e, ").")
  }

  if (!is.null(s)) data <- dplyr::filter(data, Date >= s)
  if (!is.null(e)) data <- dplyr::filter(data, Date <= e)

  data
}


#' @keywords internal
#' @noRd
.compute_forecast_dates <- function(last_date, horizon, avg_gap) {
  # Hybrid: dispatch on average gap (not period) so user-supplied 'period'
  # cannot misalign the calendar. Returns horizon dates after last_date.
  if (avg_gap <= 3) {
    # Daily / trading-day data: skip Sat/Sun. %u is locale-independent.
    candidate <- seq.Date(last_date + 1, by = "day",
                          length.out = horizon * 2)
    wday <- as.integer(format(candidate, "%u"))
    out  <- candidate[wday < 6][seq_len(horizon)]
  } else if (avg_gap <= 10) {
    out <- seq.Date(last_date, by = "week",
                    length.out = horizon + 1)[-1]
  } else if (avg_gap <= 45) {
    out <- seq.Date(last_date, by = "month",
                    length.out = horizon + 1)[-1]
  } else if (avg_gap <= 120) {
    out <- seq.Date(last_date, by = "3 months",
                    length.out = horizon + 1)[-1]
  } else {
    # Yearly+ or unusual cadence: just step by rounded average gap.
    out <- last_date + seq_len(horizon) * max(1L, round(avg_gap))
  }
  out
}


# ----------------------------------------------------------------------------
# forecast_stock - the only public function in this module
# ----------------------------------------------------------------------------

#' Forecast a Stock Series via auto.arima
#'
#' @description
#' Extracts a univariate series (single symbol + column) from the input,
#' fits \code{forecast::auto.arima()} on it, and returns the fitted model,
#' the raw forecast object and a ggplot showing historical values plus
#' the forecast with confidence bands.
#'
#' Accepts the long data.frame from \code{get_stock()/clean_stock()} or the
#' wide xts from \code{convert_stock(to = "xts")}. Other input types are
#' rejected.
#'
#' @param data Long data.frame (Date/Symbol/Column/Value) or wide xts.
#' @param symbol character. Exactly one symbol. Required.
#' @param column character. Series to forecast. Default: "Close".
#' @param horizon integer (>0). Number of steps to forecast. Default: 12.
#'   \code{horizon = 0} or negative values raise an error.
#' @param ci_levels numeric vector of percentages in (0, 100). Confidence
#'   interval levels. Values outside the range are dropped with a warning;
#'   if all are invalid, an error is raised. Default: c(80, 95).
#' @param period integer or NULL. Seasonality period passed to \code{ts()}.
#'   NULL = auto-detect based on average gap between consecutive
#'   observations (<=2d -> 5; <=10d -> 5; <=35d -> 12; <=100d -> 4; else 1).
#' @param start,end character or NULL. Optional "YYYY-MM-DD" bounds that
#'   restrict the fit window. NULL = use the full available range.
#'
#' @return A list with three elements:
#' \describe{
#'   \item{model}{The fitted \code{Arima} object (call \code{summary()} on
#'     it for the standard auto.arima summary).}
#'   \item{plot}{A \code{ggplot} with historical line, forecast mean and
#'     multi-level confidence ribbons on a real date axis.}
#'   \item{forecast}{The raw \code{forecast} object with \code{$mean},
#'     \code{$lower}, \code{$upper}, \code{$level}.}
#' }
#'
#' @examples
#' \dontrun{
#' res <- forecast_stock(MIS_data, symbol = "AAPL", horizon = 30)
#' summary(res$model)        # auto.arima summary
#' res$plot                  # forecast chart
#' res$forecast$mean         # raw forecast vector
#' res$forecast$lower        # CI lower bounds matrix (one column per level)
#'
#' # Fit window only:
#' forecast_stock(MIS_data, symbol = "AAPL",
#'                start = "2022-01-01", end = "2023-12-31",
#'                horizon = 60, ci_levels = c(50, 80, 95))
#' }
#'
#' @importFrom forecast auto.arima forecast
#' @importFrom ggplot2 ggplot aes geom_line geom_ribbon geom_vline labs
#'   theme_minimal scale_color_manual
#' @importFrom dplyr filter arrange
#' @importFrom tidyr pivot_longer separate
#' @importFrom anytime anydate
#' @importFrom stringr str_trim
#' @importFrom zoo index
#' @importFrom stats ts
#' @importFrom rlang .data
#' @export
forecast_stock <- function(data,
                           symbol    = NULL,
                           column    = "Close",
                           horizon   = 12,
                           ci_levels = c(80, 95),
                           period    = NULL,
                           start     = NULL,
                           end       = NULL) {

  # ---- 1. Input validation -------------------------------------------------

  if (is.null(symbol)) {
    stop("'symbol' is required. Specify exactly one symbol.")
  }
  if (length(symbol) > 1) {
    stop("'symbol' must be a single value. ",
         "forecast_stock() forecasts one series at a time.")
  }

  if (!is.numeric(horizon) || length(horizon) != 1 || horizon <= 0) {
    stop("'horizon' must be a positive integer (got: ",
         paste(horizon, collapse = ", "), ").")
  }
  horizon <- as.integer(horizon)

  if (!is.numeric(ci_levels) || length(ci_levels) == 0) {
    stop("'ci_levels' must be a non-empty numeric vector, e.g. c(80, 95).")
  }
  valid_ci <- ci_levels[ci_levels > 0 & ci_levels < 100]
  if (length(valid_ci) == 0) {
    stop("All 'ci_levels' values were outside (0, 100). Provide at least ",
         "one valid level, e.g. 80 or 95.")
  }
  if (length(valid_ci) != length(ci_levels)) {
    warning("Some 'ci_levels' values were outside (0, 100) and were ignored.")
    ci_levels <- valid_ci
  }

  # ---- 2. Normalize input type --------------------------------------------

  data <- .to_long_df(data)

  # ---- 3. Filter fit window -----------------------------------------------

  data <- .filter_dates(data, start, end)

  # ---- 4. Extract univariate series ---------------------------------------

  filtered <- dplyr::filter(data, Symbol == symbol, Column == column)

  if (nrow(filtered) == 0) {
    stop("No data found for symbol: ", symbol, ", column: ", column,
         if (!is.null(start) || !is.null(end))
           paste0(" within [", if (is.null(start)) "min" else start,
                  " .. ",        if (is.null(end))   "max" else end, "]")
         else "")
  }

  filtered <- filtered[order(filtered$Date), ]
  values   <- filtered$Value
  dates    <- filtered$Date

  if (any(is.na(values))) {
    stop("Series contains NA values. Run clean_stock() before forecast_stock().")
  }
  if (length(values) < 10) {
    stop("Not enough observations for ARIMA. Got: ", length(values),
         ". Need at least 10.")
  }

  # ---- 5. Period auto-detect ----------------------------------------------

  avg_gap <- as.numeric(mean(diff(dates)))

  if (is.null(period)) {
    if (avg_gap <= 2) {
      period <- 5
    } else if (avg_gap <= 10) {
      period <- 5
    } else if (avg_gap <= 35) {
      period <- 12
    } else if (avg_gap <= 100) {
      period <- 4
    } else {
      period <- 1
    }
    message("Auto-detected period: ", period,
            " (avg gap = ", round(avg_gap, 2), " days)")
  }

  # ---- 6. Fit ARIMA -------------------------------------------------------

  ts_obj <- stats::ts(values, frequency = period)
  message("Fitting auto.arima model...")
  model <- forecast::auto.arima(ts_obj)

  a <- model$arma   # c(p, q, P, Q, period, d, D)
  model_str <- paste0("ARIMA(", a[1], ",", a[6], ",", a[2], ")",
    if (period > 1)
      paste0("(", a[3], ",", a[7], ",", a[4], ")[", period, "]")
    else "")
  message("Model: ", model_str)

  # ---- 7. Forecast --------------------------------------------------------

  fc <- forecast::forecast(model, h = horizon, level = ci_levels)

  forecast_dates <- .compute_forecast_dates(
    last_date = dates[length(dates)],
    horizon   = horizon,
    avg_gap   = avg_gap
  )

  # ---- 8. Build ggplot ----------------------------------------------------

  hist_df <- data.frame(
    Date  = dates,
    value = values,
    type  = "Historical",
    stringsAsFactors = FALSE
  )
  fc_df <- data.frame(
    Date  = forecast_dates,
    value = as.numeric(fc$mean),
    type  = "Forecast",
    stringsAsFactors = FALSE
  )
  combined <- rbind(hist_df, fc_df)
  combined$type <- factor(combined$type, levels = c("Historical","Forecast"))

  ribbon_df <- data.frame(Date = forecast_dates,
                          stringsAsFactors = FALSE)
  for (i in seq_along(ci_levels)) {
    ribbon_df[[paste0("lower_", ci_levels[i])]] <- as.numeric(fc$lower[, i])
    ribbon_df[[paste0("upper_", ci_levels[i])]] <- as.numeric(fc$upper[, i])
  }

  # Draw widest ribbon first (lowest alpha), narrowest last (highest alpha)
  order_ci  <- order(ci_levels, decreasing = TRUE)
  alphas    <- seq(0.15, 0.35, length.out = length(ci_levels))

  p <- ggplot2::ggplot()

  for (k in seq_along(order_ci)) {
    i <- order_ci[k]
    lo <- paste0("lower_", ci_levels[i])
    hi <- paste0("upper_", ci_levels[i])
    p <- p + ggplot2::geom_ribbon(
      data = ribbon_df,
      ggplot2::aes(
        x    = Date,
        ymin = .data[[lo]],
        ymax = .data[[hi]]
      ),
      fill  = "steelblue",
      alpha = alphas[k]
    )
  }

  p <- p +
    ggplot2::geom_line(
      data = combined,
      ggplot2::aes(x = Date, y = value, color = type),
      linewidth = 0.7
    ) +
    ggplot2::geom_vline(
      xintercept = dates[length(dates)],
      linetype   = "dashed",
      color      = "grey50"
    ) +
    ggplot2::scale_color_manual(
      values = c("Historical" = "steelblue", "Forecast" = "tomato")
    ) +
    ggplot2::labs(
      title    = paste("Forecast -", symbol, "/", column),
      subtitle = paste0(model_str,
                        "  |  horizon: ", horizon,
                        "  |  CI: ", paste0(ci_levels, "%", collapse = ", ")),
      x = "Date",
      y = column,
      color = NULL
    ) +
    ggplot2::theme_minimal()

  # ---- 9. Return ----------------------------------------------------------

  list(
    model    = model,
    plot     = p,
    forecast = fc
  )
}
