#' MISdata: Market Intelligence Suite for R
#'
#' MISdata provides an end-to-end pipeline for stock-market data workflows
#' written in pure R. It fetches index constituents from Wikipedia, pulls
#' OHLCV history from Yahoo Finance, and then offers NA cleaning,
#' time-series format conversion, period aggregation, exploratory data
#' analysis (line plots, ACF / PACF, STL decomposition) and ARIMA-based
#' forecasting.
#'
#' The package is built around two design principles. First, the canonical
#' in-memory representation is a \strong{long data.frame} with the columns
#' \code{Date}, \code{Symbol}, \code{Column}, \code{Value} — produced by
#' \code{\link{get_stock}} and consumed by every downstream function.
#' Second, time-series interoperability is delegated to \pkg{tsbox} so that
#' \code{xts}, \code{tsibble}, \code{zoo}, \code{ts} and \code{data.frame}
#' can be converted to one another through a single bridge in
#' \code{\link{convert_stock}}.
#'
#' @section Pipeline overview:
#' \preformatted{
#'   get_stock        ->  long data.frame (Date / Symbol / Column / Value)
#'        |
#'   clean_stock      ->  group-wise NA handling (long df)
#'        |
#'   convert_stock    ->  xts / tsibble / zoo / ts / df (long or wide)
#'        |
#'   change_period    ->  daily / monthly / quarterly / yearly (OHLCV-aware)
#'        |
#'    +---+--------------------------+
#'    |                              |
#'   eda                          forecast
#'    plot_stock                    forecast_stock
#'    get_acf / get_pacf             (list of model / plot / forecast)
#'    plot_seasonal
#' }
#'
#' @section Main functions:
#' \describe{
#'   \item{Data acquisition}{
#'     \code{\link{get_index_components}} — scrape DJI or S&P 500 component
#'     lists from Wikipedia.
#'     \code{\link{sample_symbols}} — reproducible random sampling of
#'     tickers.
#'     \code{\link{get_stock}} — fetch OHLCV history from Yahoo Finance and
#'     return it as a long data.frame.}
#'   \item{Cleaning}{
#'     \code{\link{clean_stock}} — group-wise NA handling with two methods:
#'     \code{"trim"} (drop leading / trailing NAs per Symbol + Column) and
#'     \code{"approx"} (linear interpolation of interior NAs).}
#'   \item{Format conversion}{
#'     \code{\link{convert_stock}} — bridge between \code{xts}, \code{tsibble},
#'     \code{zoo}, \code{ts} and \code{data.frame} in long or wide layout
#'     using \pkg{tsbox}.}
#'   \item{Period aggregation}{
#'     \code{\link{change_period}} — aggregate daily series to monthly /
#'     quarterly / yearly with the financial OHLCV convention (Open = first,
#'     High = max, Low = min, Close = last, Volume = sum). A user-supplied
#'     \code{aggregate_fn} overrides this and applies a single function to
#'     every column.}
#'   \item{Exploratory analysis}{
#'     \code{\link{plot_stock}} — multi-symbol time series chart (max 5
#'     symbols, optional date window).
#'     \code{\link{get_acf}} / \code{\link{get_pacf}} — ACF / PACF values
#'     for one or more symbols, optionally with a faceted bar-chart.
#'     \code{\link{plot_seasonal}} — STL decomposition of a single
#'     symbol's series with auto-detected seasonality period.}
#'   \item{Forecasting}{
#'     \code{\link{forecast_stock}} — fit \code{forecast::auto.arima()} on
#'     a single symbol's series and return the fitted model, the raw
#'     forecast object and a ggplot with multi-level confidence ribbons on
#'     a real date axis.}
#' }
#'
#' @section Accepted input types:
#' The analysis functions (\code{plot_stock}, \code{get_acf},
#' \code{get_pacf}, \code{plot_seasonal}, \code{forecast_stock}) accept
#' two input shapes:
#' \itemize{
#'   \item the long \code{data.frame} returned by \code{\link{get_stock}}
#'         and \code{\link{clean_stock}};
#'   \item the wide \code{xts} produced by
#'         \code{\link{convert_stock}}\code{(to = "xts")}, in which case
#'         column names of the form \code{"Symbol_Column"} are split back
#'         into the canonical long layout internally.
#' }
#' Other types (\code{tsibble}, \code{zoo}, \code{ts}) must be converted
#' first through \code{\link{convert_stock}}.
#'
#' @section Quick start:
#' \preformatted{
#'   library(MISdata)
#'
#'   # 1. Build a universe of 3 random Dow Jones tickers
#'   dji   <- get_index_components("DJI")
#'   syms  <- sample_symbols(dji, n = 3, seed = 42)
#'
#'   # 2. Fetch OHLCV
#'   data  <- get_stock(syms,
#'                      start   = "2022-01-01",
#'                      columns = c("Close", "Volume"))
#'
#'   # 3. Clean any NAs (group-wise)
#'   clean <- clean_stock(data)
#'
#'   # 4. Visualize trends
#'   plot_stock(clean, format = "long", column = "Close")
#'
#'   # 5. Autocorrelation structure
#'   acf_res <- get_acf(clean, symbols = syms, plot = TRUE)
#'   acf_res$plot
#'
#'   # 6. Aggregate to monthly (OHLCV-aware) and forecast a year ahead
#'   monthly <- change_period(clean, period = "monthly")
#'   fc      <- forecast_stock(monthly, symbol = syms[1], horizon = 12)
#'
#'   summary(fc$model)        # auto.arima model summary
#'   fc$plot                  # forecast chart with CI ribbons
#'   fc$forecast$mean         # raw point forecasts
#' }
#'
#' @section Design constraints:
#' \itemize{
#'   \item Pure R (no Python, no Selenium); only static HTML scraping
#'         via \pkg{rvest}.
#'   \item No outlier detection (out of scope; user-controlled if needed).
#'   \item Function names are descriptive (no prefix); the canonical
#'         in-memory object is \code{MIS_data} (raw long df) or
#'         \code{MIS_tsdata} (after \code{convert_stock}).
#'   \item Native pipe \code{|>} (R >= 4.1.0) is used throughout — no
#'         \pkg{magrittr} dependency.
#' }
#'
#' @section Author / Maintainer:
#' MISDataGit team — see the \code{DESCRIPTION} file for the current list
#' of authors and contributors.
#'
#' @keywords internal
"_PACKAGE"


# ---- NSE global variable bindings -----------------------------------------
# Suppress R CMD CHECK NOTEs of the form
#   "no visible binding for global variable 'X'"
# that arise from dplyr::filter() / dplyr::mutate() and tidyr::pivot_*()
# NSE inside the package.
utils::globalVariables(c(
  # core long-format schema
  "Date", "Symbol", "Column", "Value",
  # tsbox canonical long schema
  "id", "time"
))
