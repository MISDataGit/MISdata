#' Clean Stock Data
#'
#' @description
#' Takes the long format data.frame returned by get_stock() and handles
#' NA values using the selected method(s).
#'
#' Two methods are available:
#' - "trim"  : Removes NA rows from the beginning and end of each
#'             Symbol + Column group.
#' - "approx": Fills interior NA values using linear interpolation.
#'             (Draws a straight line between two known points.)
#'
#' Both methods can be applied together: na_method = c("trim", "approx")
#' Order matters: trim is always applied before approx.
#'
#' Should be called BEFORE convert_stock().
#'
#' @param data data.frame. Long format output from get_stock() with columns:
#'   Date, Symbol, Column, Value. (required)
#' @param na_method character vector. Cleaning method(s) to apply.
#'   "trim", "approx", or both: c("trim", "approx").
#'   Default: c("trim", "approx").
#'
#' @return Cleaned data.frame in the same long format. Column names are preserved.
#'
#' @examples
#' \dontrun{
#' MIS_data <- get_stock(c("AAPL", "MSFT"), start = "2020-01-01")
#'
#' # Apply both methods (default)
#' MIS_data <- clean_stock(MIS_data)
#'
#' # Trim only
#' MIS_data <- clean_stock(MIS_data, na_method = "trim")
#'
#' # Approx only
#' MIS_data <- clean_stock(MIS_data, na_method = "approx")
#' }
#'
#' @importFrom zoo na.approx
#' @importFrom dplyr bind_rows
#' @export
clean_stock <- function(data, na_method = c("trim", "approx")) {

  # --- Input Validation ---
  if (!is.data.frame(data)) {
    stop("'data' must be a data.frame in long format. Use the output of get_stock().")
  }

  required_cols <- c("Date", "Symbol", "Column", "Value")
  missing_cols  <- setdiff(required_cols, colnames(data))
  if (length(missing_cols) > 0) {
    stop(paste("Missing columns:", paste(missing_cols, collapse = ", ")))
  }

  if (nrow(data) == 0) {
    stop("'data' is empty.")
  }

  valid_methods <- c("trim", "approx")
  invalid <- setdiff(na_method, valid_methods)
  if (length(invalid) > 0) {
    stop(paste(
      "'na_method' must be 'trim' and/or 'approx'.",
      "Invalid value(s):", paste(invalid, collapse = ", ")
    ))
  }

  # --- Clean Each Symbol + Column Group Separately ---
  # Split data into groups: e.g. AAPL/Close, AAPL/Volume, MSFT/Close
  # Each group is an independent time series and cleaned on its own
  groups <- split(data, list(data$Symbol, data$Column), drop = TRUE)

  cleaned_groups <- lapply(groups, function(grp) {

    # Sort by date within group — critical for time series operations
    grp <- grp[order(grp$Date), ]
    values <- grp$Value

    # --- Step 1: trim ---
    # Find first and last non-NA position
    # Remove everything before the first valid value (leading NAs)
    # Remove everything after the last valid value (trailing NAs)
    if ("trim" %in% na_method) {
      non_na_idx <- which(!is.na(values))

      if (length(non_na_idx) == 0) {
        # Entire group is NA — drop it
        return(NULL)
      }

      first_valid <- min(non_na_idx)
      last_valid  <- max(non_na_idx)
      grp    <- grp[first_valid:last_valid, ]
      values <- grp$Value
    }

    # --- Step 2: approx ---
    # Fill interior NAs using linear interpolation
    # na.rm = FALSE: if an NA cannot be filled, leave it (don't remove the row)
    if ("approx" %in% na_method) {
      grp$Value <- zoo::na.approx(values, na.rm = FALSE)
    }

    return(grp)
  })

  # Drop groups that were entirely NA (returned NULL by trim)
  cleaned_groups <- Filter(Negate(is.null), cleaned_groups)

  if (length(cleaned_groups) == 0) {
    stop("All data was NA after cleaning. Check your input.")
  }

  # Combine all groups back into one long data.frame
  result <- dplyr::bind_rows(cleaned_groups)
  result <- result[order(result$Date, result$Symbol, result$Column), ]
  rownames(result) <- NULL

  # Report remaining NAs if any
  na_remaining <- sum(is.na(result$Value))
  if (na_remaining > 0) {
    warning(na_remaining, " NA(s) could not be filled and remain in the data.")
  }

  message("clean_stock done. Rows: ", nrow(result),
          " | NA remaining: ", na_remaining)

  return(result)
}