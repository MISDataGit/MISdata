# ==============================================================================
# eda.R - Exploratory Data Analysis
#
# Public functions
# ----------------
#   plot_stock     : trend / time-series line chart for up to 5 symbols
#   get_acf        : ACF values (+ optional faceted plot) for up to 5 symbols
#   get_pacf       : PACF values (+ optional faceted plot) for up to 5 symbols
#   plot_seasonal  : STL decomposition (single symbol)
#
# Accepted input types (all functions):
#   - long data.frame  : Date / Symbol / Column / Value (get_stock() output)
#   - wide xts         : convert_stock(to = "xts") output; columns encoded as
#                        "Symbol_Column" (e.g. AAPL_Close); normalized to a
#                        canonical long df internally.
# ==============================================================================


# ----------------------------------------------------------------------------
# Private helpers (not exported)
# ----------------------------------------------------------------------------

#' @keywords internal
#' @noRd
.to_long_df <- function(data) {

  # Already in long format from get_stock()/clean_stock()?
  if (is.data.frame(data) &&
      all(c("Date", "Symbol", "Column", "Value") %in% colnames(data))) {
    return(as.data.frame(data))
  }

  # Wide xts from convert_stock(to = "xts")?
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

    # If a column had no underscore (e.g. plain "Close"), Column becomes NA;
    # treat the whole name as Symbol with a placeholder Column.
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
.compute_acf_pacf <- function(data, symbols, column, lag_max,
                              type = c("acf", "pacf")) {

  type <- match.arg(type)
  data <- .to_long_df(data)

  if (is.null(symbols) || length(symbols) == 0) {
    stop("'symbols' is required. Provide one or more symbols.")
  }

  if (length(symbols) > 5) {
    warning("More than 5 symbols provided. Only the first 5 will be used.")
    symbols <- symbols[1:5]
  }

  results <- list()
  for (sym in symbols) {
    filtered <- dplyr::filter(data, Symbol == sym, Column == column)
    if (nrow(filtered) == 0) {
      stop("No data found for symbol: ", sym, " and column: ", column)
    }
    filtered <- filtered[order(filtered$Date), ]
    values   <- filtered$Value

    if (any(is.na(values))) {
      stop("NA values in series for symbol '", sym,
           "'. Run clean_stock() before ", toupper(type), "computation.")
    }

    if (length(values) < 3) {
      stop("At least three observations are required to analyze returns for ",
           "symbol '", sym, "'.")
    }

    returns <- diff(values) / values[-length(values)]
    if (any(!is.finite(returns))) {
      stop("Returns for symbol '", sym,
           "' contain non-finite values. Check the selected series for zeros ",
           "or non-finite observations.")
    }

    if (type == "acf") {
      res  <- stats::acf(returns, lag.max = lag_max, plot = FALSE)
      vals <- as.numeric(res$acf)[-1]   # drop lag 0 (always 1.0)
    } else {
      res  <- stats::pacf(returns, lag.max = lag_max, plot = FALSE)
      vals <- as.numeric(res$acf)
    }

    results[[sym]] <- data.frame(
      lag    = seq_along(vals),
      val    = vals,
      symbol = sym,
      stringsAsFactors = FALSE
    )
  }

  out <- do.call(rbind, results)
  rownames(out) <- NULL
  names(out)[names(out) == "val"] <- type
  out
}


# ----------------------------------------------------------------------------
# 1. plot_stock
# ----------------------------------------------------------------------------

#' Plot Stock Time Series
#'
#' @description
#' Draws a line chart of one or more stock series. Accepts the long data.frame
#' from \code{get_stock()/clean_stock()} or a wide \code{xts} from
#' \code{convert_stock(to = "xts")}. Maximum 5 symbols.
#'
#' @param data Long data.frame (Date/Symbol/Column/Value) or wide xts.
#' @param symbols character vector or NULL. Which symbols to plot. NULL = all
#'   symbols in the data (capped at 5). Default: NULL.
#' @param column character. Which series to plot ("Close", "Volume", ...).
#'   Default: "Close".
#' @param format character. "wide" = single panel, colored lines.
#'   "long" = one panel per symbol (facet_wrap). Default: "wide".
#' @param start,end character or NULL. Optional "YYYY-MM-DD" bounds for the
#'   plotted window. NULL = use full available range. Default: NULL.
#' @param title character or NULL. Chart title. NULL = auto-generated.
#'
#' @return A ggplot object.
#'
#' @examples
#' \dontrun{
#' plot_stock(MIS_data, format = "long")
#' plot_stock(MIS_data, symbols = c("AAPL","MSFT"),
#'            start = "2023-01-01", end = "2023-06-30")
#' }
#'
#' @importFrom ggplot2 ggplot aes geom_line facet_wrap labs theme_minimal
#' @importFrom dplyr filter
#' @importFrom tidyr pivot_longer separate
#' @importFrom anytime anydate
#' @importFrom stringr str_trim
#' @importFrom zoo index
#' @export
plot_stock <- function(data,
                       symbols = NULL,
                       column  = "Close",
                       format  = "wide",
                       start   = NULL,
                       end     = NULL,
                       title   = NULL) {

  data <- .to_long_df(data)

  if (!format %in% c("long", "wide")) {
    stop("'format' must be 'long' or 'wide'.")
  }

  data <- .filter_dates(data, start, end)

  if (is.null(symbols)) {
    symbols <- unique(data$Symbol)
  }

  if (length(symbols) > 5) {
    warning("More than 5 symbols provided. Only the first 5 will be plotted.")
    symbols <- symbols[1:5]
  }

  plot_data <- dplyr::filter(data, Symbol %in% symbols, Column == column)

  if (nrow(plot_data) == 0) {
    stop("No data found for column: ", column,
         " and symbols: ", paste(symbols, collapse = ", "),
         if (!is.null(start) || !is.null(end))
           paste0(" within [", start %||% "min", " .. ", end %||% "max", "]")
         else "")
  }

  if (is.null(title)) {
    title <- paste(column, "-", paste(symbols, collapse = ", "))
  }

  p <- ggplot2::ggplot(
    plot_data,
    ggplot2::aes(x = Date, y = Value, color = Symbol)
  ) +
    ggplot2::geom_line() +
    ggplot2::labs(title = title, x = "Date", y = column, color = "Symbol") +
    ggplot2::theme_minimal()

  if (format == "long") {
    p <- p + ggplot2::facet_wrap(~ Symbol, scales = "free_y")
  }

  p
}

# tiny null-coalescer used above
`%||%` <- function(a, b) if (is.null(a)) b else a


# ----------------------------------------------------------------------------
# 2. get_acf
# ----------------------------------------------------------------------------

#' Autocorrelation Function (ACF)
#'
#' @description
#' Calculates simple returns from the selected series and computes ACF values
#' for one or more symbols. Results are returned as a long data.frame, with an
#' optional faceted bar-chart visualization. Returns are calculated as
#' \eqn{r_t = (x_t / x_{t-1}) - 1}.
#'
#' @param data Long data.frame (Date/Symbol/Column/Value) or wide xts.
#' @param symbols character vector. One or more symbols (max 5). Required.
#' @param column character. Series from which simple returns are calculated.
#'   Default: "Close".
#' @param lag_max integer. Maximum lag. Default: 40.
#' @param plot logical. If TRUE, returns a list with both the table and a
#'   faceted ggplot. Default: FALSE (table only).
#'
#' @return
#'   \code{plot = FALSE}: data.frame with columns \code{lag, acf, symbol}.
#'   \code{plot = TRUE} : \code{list(table = data.frame, plot = ggplot)}.
#'
#' @examples
#' \dontrun{
#' tbl <- get_acf(MIS_data, symbols = c("AAPL","MSFT"), lag_max = 30)
#' res <- get_acf(MIS_data, symbols = "AAPL", plot = TRUE)
#' res$table
#' res$plot
#' }
#'
#' @importFrom ggplot2 ggplot aes geom_col facet_wrap labs theme_minimal
#' @importFrom dplyr filter
#' @importFrom stats acf
#' @export
get_acf <- function(data,
                    symbols = NULL,
                    column  = "Close",
                    lag_max = 40,
                    plot    = FALSE) {

  tbl <- .compute_acf_pacf(data, symbols, column, lag_max, type = "acf")

  if (!plot) return(tbl)

  p <- ggplot2::ggplot(tbl, ggplot2::aes(x = lag, y = acf)) +
    ggplot2::geom_col(width = 0.5) +
    ggplot2::facet_wrap(~ symbol) +
    ggplot2::labs(title = paste("ACF of", column, "Returns"),
                  x = "Lag", y = "ACF") +
    ggplot2::theme_minimal()

  list(table = tbl, plot = p)
}


# ----------------------------------------------------------------------------
# 3. get_pacf
# ----------------------------------------------------------------------------

#' Partial Autocorrelation Function (PACF)
#'
#' @description
#' Calculates simple returns from the selected series and computes PACF values
#' for one or more symbols. Results are returned as a long data.frame, with an
#' optional faceted bar-chart visualization. Returns are calculated as
#' \eqn{r_t = (x_t / x_{t-1}) - 1}.
#'
#' @param data Long data.frame (Date/Symbol/Column/Value) or wide xts.
#' @param symbols character vector. One or more symbols (max 5). Required.
#' @param column character. Series from which simple returns are calculated.
#'   Default: "Close".
#' @param lag_max integer. Maximum lag. Default: 40.
#' @param plot logical. If TRUE, returns a list with both the table and a
#'   faceted ggplot. Default: FALSE (table only).
#'
#' @return
#'   \code{plot = FALSE}: data.frame with columns \code{lag, pacf, symbol}.
#'   \code{plot = TRUE} : \code{list(table = data.frame, plot = ggplot)}.
#'
#' @examples
#' \dontrun{
#' tbl <- get_pacf(MIS_data, symbols = c("AAPL","MSFT"), lag_max = 30)
#' res <- get_pacf(MIS_data, symbols = "AAPL", plot = TRUE)
#' res$plot
#' }
#'
#' @importFrom ggplot2 ggplot aes geom_col facet_wrap labs theme_minimal
#' @importFrom dplyr filter
#' @importFrom stats pacf
#' @export
get_pacf <- function(data,
                     symbols = NULL,
                     column  = "Close",
                     lag_max = 40,
                     plot    = FALSE) {

  tbl <- .compute_acf_pacf(data, symbols, column, lag_max, type = "pacf")

  if (!plot) return(tbl)

  p <- ggplot2::ggplot(tbl, ggplot2::aes(x = lag, y = pacf)) +
    ggplot2::geom_col(width = 0.5) +
    ggplot2::facet_wrap(~ symbol) +
    ggplot2::labs(title = paste("PACF of", column, "Returns"),
                  x = "Lag", y = "PACF") +
    ggplot2::theme_minimal()

  list(table = tbl, plot = p)
}


# ----------------------------------------------------------------------------
# 4. plot_seasonal
# ----------------------------------------------------------------------------

#' Seasonal Decomposition Plot (STL)
#'
#' @description
#' Decomposes a single symbol's time series into Observed / Trend / Seasonal /
#' Remainder components via \code{stats::stl()} and plots all four in a
#' stacked facet.
#'
#' @param data Long data.frame (Date/Symbol/Column/Value) or wide xts.
#' @param symbol character. Exactly one symbol. Required.
#' @param column character. Series to decompose. Default: "Close".
#' @param period integer or NULL. Seasonality period. NULL = auto-detect via
#'   the average gap between consecutive observations:
#'   \itemize{
#'     \item gap <=   3 days -> 5  (daily/trading-day  -> weekly)
#'     \item gap <=  10 days -> 52 (weekly             -> yearly)
#'     \item gap <=  45 days -> 12 (monthly            -> yearly)
#'     \item gap <= 120 days -> 4  (quarterly)
#'     \item gap >  120 days -> 1  (yearly+  -> no seasonality)
#'   }
#' @param start,end character or NULL. Optional "YYYY-MM-DD" bounds.
#'   NULL = use full available range. When the filtered window contains fewer
#'   than \code{4 * period} observations, a warning is emitted because STL
#'   quality degrades on short series.
#'
#' @return A ggplot object with 4 stacked facets.
#'
#' @examples
#' \dontrun{
#' plot_seasonal(MIS_data, symbol = "AAPL")
#' plot_seasonal(MIS_data, symbol = "AAPL",
#'               start = "2022-01-01", end = "2023-12-31", period = 5)
#' }
#'
#' @importFrom ggplot2 ggplot aes geom_line facet_wrap labs theme_minimal
#' @importFrom dplyr filter
#' @importFrom tidyr pivot_longer
#' @importFrom stats stl ts
#' @export
plot_seasonal <- function(data,
                          symbol = NULL,
                          column = "Close",
                          period = NULL,
                          start  = NULL,
                          end    = NULL) {

  data <- .to_long_df(data)

  if (is.null(symbol)) {
    stop("'symbol' is required. Specify exactly one symbol.")
  }
  if (length(symbol) > 1) {
    stop("'symbol' must be a single value. ",
         "plot_seasonal() works on one symbol at a time.")
  }

  data <- .filter_dates(data, start, end)

  filtered <- dplyr::filter(data, Symbol == symbol, Column == column)
  if (nrow(filtered) == 0) {
    stop("No data found for symbol: ", symbol, " and column: ", column,
         if (!is.null(start) || !is.null(end))
           paste0(" within [", start %||% "min", " .. ", end %||% "max", "]")
         else "")
  }

  filtered <- filtered[order(filtered$Date), ]
  values   <- filtered$Value

  if (any(is.na(values))) {
    stop("Data contains NA values. Run clean_stock() before plot_seasonal().")
  }

  # ---- Auto-detect period (avg-gap based) ----
  if (is.null(period)) {
    if (nrow(filtered) < 2) {
      stop("Need at least 2 observations to auto-detect period.")
    }
    avg_gap <- as.numeric(mean(diff(filtered$Date)))

    if (avg_gap <= 3) {
      period <- 5
    } else if (avg_gap <= 10) {
      period <- 52
    } else if (avg_gap <= 45) {
      period <- 12
    } else if (avg_gap <= 120) {
      period <- 4
    } else {
      period <- 1
    }
    message("Auto-detected period: ", period,
            " (avg gap = ", round(avg_gap, 2), " days)")
  }

  if (length(values) < 2 * period) {
    stop("Not enough data for STL decomposition. ",
         "Need at least ", 2 * period, " observations. Got: ", length(values))
  }

  if (length(values) < 4 * period) {
    warning("Short window: ", length(values),
            " observations for period ", period,
            ". STL decomposition quality may be poor.")
  }

  ts_obj     <- stats::ts(values, frequency = period)
  stl_result <- stats::stl(ts_obj, s.window = "periodic")

  stl_df <- data.frame(
    Date      = filtered$Date,
    Observed  = values,
    Trend     = as.numeric(stl_result$time.series[, "trend"]),
    Seasonal  = as.numeric(stl_result$time.series[, "seasonal"]),
    Remainder = as.numeric(stl_result$time.series[, "remainder"])
  )

  stl_long <- tidyr::pivot_longer(
    stl_df, cols = -Date, names_to = "Component", values_to = "Value"
  )
  stl_long$Component <- factor(
    stl_long$Component,
    levels = c("Observed", "Trend", "Seasonal", "Remainder")
  )

  ggplot2::ggplot(stl_long, ggplot2::aes(x = Date, y = Value)) +
    ggplot2::geom_line(color = "steelblue") +
    ggplot2::facet_wrap(~ Component, ncol = 1, scales = "free_y") +
    ggplot2::labs(title = paste("Seasonal Decomposition -", symbol),
                  x = "Date", y = "") +
    ggplot2::theme_minimal()
}
