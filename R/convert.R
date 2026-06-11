#' Convert Stock Data Between Time Series Formats
#'
#' @description
#' Bridges multiple time series object types using the \code{tsbox} package.
#' Accepts \code{xts}, \code{tsibble}, \code{zoo}, \code{ts}, or \code{data.frame}
#' inputs and converts them to the target type in either long or wide layout.
#' Round-trip conversions are fully supported: for example, data converted to
#' \code{tsibble} or \code{zoo} can be converted back to \code{xts} via
#' \code{convert_stock(data, to = "xts")}.
#'
#' If the input is the long data.frame returned by \code{get_stock()} (columns:
#' \code{Date}, \code{Symbol}, \code{Column}, \code{Value}), \code{Symbol} and
#' \code{Column} are concatenated as \code{"Symbol_Column"} and the data is
#' pivoted into a wide \code{xts} object first. All subsequent conversions are
#' then done from this canonical wide \code{xts} via tsbox, which is the only
#' tsbox path that converts reliably across all target types.
#'
#' Should be called AFTER \code{clean_stock()}.
#'
#' @param data Input object. One of: \code{xts}, \code{tsibble}, \code{zoo},
#'   \code{ts}, or \code{data.frame} (long format from \code{get_stock()}).
#' @param to character. Target type. One of: "xts", "tsibble", "zoo", "ts", "df".
#'   Default: "tsibble".
#' @param format character. Output layout. Either "long" (for ggplot faceting)
#'   or "wide" (one series per column). Default: "wide".
#'   Only \code{"tsibble"} supports a true long layout (best for ggplot
#'   faceting); for \code{"df"}, \code{"xts"}, \code{"zoo"} and \code{"ts"}
#'   the layout is always wide and \code{format="long"} is ignored with a
#'   warning. The rationale: \code{get_stock()} already returns a long
#'   data.frame, so \code{convert_stock(to="df")} only adds value as a wide
#'   pivot.
#'
#' @return Converted time series object of the requested type and layout.
#'
#' @examples
#' \dontrun{
#' MIS_data    <- get_stock(c("AAPL", "MSFT"), start = "2020-01-01")
#'
#' # data.frame -> tsibble (wide)
#' MIS_tsdata  <- convert_stock(MIS_data, to = "tsibble", format = "wide")
#'
#' # data.frame -> long data.frame (faceting)
#' MIS_long    <- convert_stock(MIS_data, to = "df", format = "long")
#'
#' # Round-trip: tsibble -> xts
#' MIS_xts     <- convert_stock(MIS_tsdata, to = "xts")
#'
#' # Round-trip: zoo -> xts
#' MIS_zoo     <- convert_stock(MIS_data, to = "zoo")
#' MIS_xts2    <- convert_stock(MIS_zoo, to = "xts")
#' }
#'
#' @importFrom tsbox ts_tsibble ts_xts ts_zoo ts_ts ts_df ts_wide
#' @importFrom tidyr pivot_wider
#' @importFrom dplyr mutate select arrange
#' @importFrom xts xts
#' @importFrom tsibble as_tsibble
#' @export
convert_stock <- function(data, to = "tsibble", format = "wide") {

  valid_to <- c("xts", "tsibble", "zoo", "ts", "df")
  if (!is.character(to) || length(to) != 1 || !(to %in% valid_to)) {
    stop(paste0(
      "'to' must be one of: ", paste(valid_to, collapse = ", "), "."
    ))
  }

  valid_format <- c("long", "wide")
  if (!is.character(format) || length(format) != 1 || !(format %in% valid_format)) {
    stop("'format' must be either 'long' or 'wide'.")
  }

  if (!inherits(data, c("xts", "tbl_ts", "zoo", "ts", "data.frame"))) {
    stop(
      "'data' must be one of: xts, tsibble, zoo, ts, or data.frame. ",
      "Got: ", paste(class(data), collapse = "/"), "."
    )
  }

  is_long_get_stock <- is.data.frame(data) &&
    all(c("Date", "Symbol", "Column", "Value") %in% colnames(data))

  if (is_long_get_stock) {
    wide_df <- data |>
      dplyr::mutate(id = paste(Symbol, Column, sep = "_")) |>
      dplyr::select(Date, id, Value) |>
      dplyr::arrange(Date, id) |>
      tidyr::pivot_wider(names_from = id, values_from = Value)

    dates <- wide_df$Date
    mat   <- as.matrix(wide_df[, -1, drop = FALSE])
    data  <- xts::xts(mat, order.by = dates)
  }

  converted <- switch(to,
    "xts" = if (inherits(data, "xts")) data else tsbox::ts_xts(data),
    "zoo" = tsbox::ts_zoo(data),
    "ts"  = tsbox::ts_ts(data),
    "df"  = tsbox::ts_wide(tsbox::ts_df(data)),
    "tsibble" = {
      if (format == "wide") {
        wide_df <- tsbox::ts_wide(tsbox::ts_df(data))
        tsibble::as_tsibble(wide_df, index = time)
      } else {
        tsbox::ts_tsibble(data)
      }
    }
  )

  if (format == "long" && to != "tsibble") {
    warning(
      "'long' layout is only supported for to='tsibble'. ",
      "to='", to, "' always returns wide layout."
    )
  }

  return(converted)
}
