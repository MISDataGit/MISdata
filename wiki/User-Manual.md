# MISdata User Manual

`MISdata` is an R package for retrieving, cleaning, transforming, analyzing,
and forecasting stock market data. It provides an end-to-end workflow from
index component discovery to ARIMA-based forecasts.

> **Project status:** Experimental, version 0.1.0. Forecasts are statistical
> estimates and should not be treated as financial advice.

## Contents

- [Requirements](#requirements)
- [Installation](#installation)
- [Quick Start](#quick-start)
- [Understanding the Data](#understanding-the-data)
- [1. Find Stock Symbols](#1-find-stock-symbols)
- [2. Download Stock Data](#2-download-stock-data)
- [3. Clean Missing Values](#3-clean-missing-values)
- [4. Change the Time Period](#4-change-the-time-period)
- [5. Convert the Data Format](#5-convert-the-data-format)
- [6. Plot Stock Prices](#6-plot-stock-prices)
- [7. Analyze Autocorrelation](#7-analyze-autocorrelation)
- [8. Decompose Seasonality](#8-decompose-seasonality)
- [9. Forecast a Stock Series](#9-forecast-a-stock-series)
- [Complete Workflow](#complete-workflow)
- [Function Reference](#function-reference)
- [Troubleshooting](#troubleshooting)

## Requirements

- R 4.1.0 or later
- An internet connection for downloading index components and stock data
- Access to the package's upstream data sources

The required R package dependencies are installed automatically when
`MISdata` is installed.

## Installation

Install the package from GitHub:

```r
install.packages("remotes")
remotes::install_github("MISDataGit/MISdata")
```

Load it in an R session:

```r
library(MISdata)
```

To confirm the installed version:

```r
packageVersion("MISdata")
```

## Quick Start

The following example downloads two stocks, cleans the data, plots closing
prices, and creates a forecast:

```r
library(MISdata)

prices <- get_stock(
  symbols = c("AAPL", "MSFT"),
  start = "2023-01-01",
  end = "2024-12-31",
  columns = c("Close", "Volume")
)

prices_clean <- clean_stock(prices)

plot_stock(
  prices_clean,
  symbols = c("AAPL", "MSFT"),
  column = "Close",
  format = "long"
)

forecast_result <- forecast_stock(
  prices_clean,
  symbol = "AAPL",
  column = "Close",
  horizon = 30
)

summary(forecast_result$model)
forecast_result$plot
```

## Understanding the Data

`get_stock()` returns a long-format `data.frame`. This is the package's main
working format.

| Column | Meaning | Example |
|---|---|---|
| `Date` | Trading date | `2024-01-02` |
| `Symbol` | Stock ticker | `AAPL` |
| `Column` | Market data field | `Close` |
| `Value` | Numeric observation | `185.64` |

Example:

| Date | Symbol | Column | Value |
|---|---|---|---:|
| 2024-01-02 | AAPL | Close | 185.64 |
| 2024-01-02 | AAPL | Volume | 82488700 |
| 2024-01-02 | MSFT | Close | 370.87 |

The available market data fields are:

- `Open`
- `High`
- `Low`
- `Close`
- `Volume`
- `Adjusted`

Run `clean_stock()` before analysis when the downloaded data contains missing
values. The analysis and forecasting functions accept either this long format
or the wide `xts` output created by `convert_stock(to = "xts")`.

## 1. Find Stock Symbols

### Get index components

`get_index_components()` retrieves the current component list for the Dow Jones
Industrial Average or S&P 500 from Wikipedia.

```r
dow_jones <- get_index_components("DJI")
sp500 <- get_index_components("SP500")

head(dow_jones)
```

The result contains:

- `Symbol`
- `Company`
- `Industry`
- `Date_Added`

Supported index names are exactly `"DJI"` and `"SP500"`.

### Sample symbols

Use `sample_symbols()` to select random tickers from an index component table:

```r
symbols <- sample_symbols(
  components = dow_jones,
  n = 3,
  seed = 42
)

symbols
```

Set `seed` when the same sample must be reproducible. If `n` is larger than the
number of available components, all available symbols are returned.

You can skip index discovery and provide ticker symbols directly:

```r
symbols <- c("AAPL", "MSFT", "NVDA")
```

## 2. Download Stock Data

Use `get_stock()` to download historical market data:

```r
prices <- get_stock(
  symbols = c("AAPL", "MSFT"),
  start = "2022-01-01",
  end = "2024-12-31",
  columns = c("Open", "High", "Low", "Close", "Volume", "Adjusted"),
  source = "yahoo"
)
```

### Main arguments

| Argument | Description | Default |
|---|---|---|
| `symbols` | Character vector of ticker symbols | Required |
| `start` | First date in `YYYY-MM-DD` format | `"2020-01-01"` |
| `end` | Last date in `YYYY-MM-DD` format | Current date |
| `columns` | Market data fields to keep | `"Close"` |
| `source` | Data source: `"yahoo"` or `"google"` | `"yahoo"` |

Yahoo Finance is the recommended default. Data-source availability and ticker
coverage are controlled by the upstream service.

If one symbol cannot be downloaded, the function warns and continues with the
remaining symbols. It stops if every requested symbol fails.

## 3. Clean Missing Values

`clean_stock()` processes each `Symbol` and `Column` series independently.

```r
prices_clean <- clean_stock(
  prices,
  na_method = c("trim", "approx")
)
```

Available methods:

| Method | Behavior |
|---|---|
| `"trim"` | Removes leading and trailing rows whose values are `NA` |
| `"approx"` | Linearly interpolates missing values inside a series |

Both methods are used by default. When both are requested, trimming is applied
before interpolation.

Examples:

```r
# Default: trim edges, then interpolate interior gaps
prices_clean <- clean_stock(prices)

# Remove only leading and trailing missing values
prices_trimmed <- clean_stock(prices, na_method = "trim")

# Interpolate only interior gaps
prices_interpolated <- clean_stock(prices, na_method = "approx")
```

If missing values remain, `clean_stock()` reports them in a warning. Resolve
remaining missing values before calling `get_acf()`, `get_pacf()`,
`plot_seasonal()`, or `forecast_stock()`.

## 4. Change the Time Period

`change_period()` aggregates a series to monthly, quarterly, or yearly
frequency.

```r
monthly <- change_period(prices_clean, period = "monthly")
quarterly <- change_period(prices_clean, period = "quarterly")
yearly <- change_period(prices_clean, period = "yearly")
```

By default, each OHLCV field is aggregated using financial conventions:

| Field | Aggregation |
|---|---|
| `Open` | First value in the period |
| `High` | Maximum value |
| `Low` | Minimum value |
| `Close` | Last value in the period |
| `Adjusted` | Last value in the period |
| `Volume` | Sum |

Apply one function to every series by setting `aggregate_fn`:

```r
monthly_mean <- change_period(
  prices_clean,
  period = "monthly",
  aggregate_fn = mean
)
```

The output type matches the input type. For example, a long `data.frame`
produces a long `data.frame`, while an `xts` input produces an `xts` result.

## 5. Convert the Data Format

`convert_stock()` converts data between common R time-series formats:

```r
prices_xts <- convert_stock(prices_clean, to = "xts")
prices_tsibble <- convert_stock(prices_clean, to = "tsibble")
prices_zoo <- convert_stock(prices_clean, to = "zoo")
prices_ts <- convert_stock(prices_clean, to = "ts")
prices_df <- convert_stock(prices_clean, to = "df")
```

Supported values for `to` are:

- `"xts"`
- `"tsibble"`
- `"zoo"`
- `"ts"`
- `"df"`

Wide outputs use names such as `AAPL_Close` and `MSFT_Volume`.

```r
prices_xts <- convert_stock(
  prices_clean,
  to = "xts",
  format = "wide"
)
```

A true long converted layout is supported only for `to = "tsibble"`:

```r
prices_long_tsibble <- convert_stock(
  prices_clean,
  to = "tsibble",
  format = "long"
)
```

For `xts`, `zoo`, `ts`, and `df`, `format = "long"` is ignored and a warning is
shown. The original output from `get_stock()` is already a long `data.frame`.

## 6. Plot Stock Prices

`plot_stock()` returns a `ggplot` object.

### One chart with multiple lines

```r
plot_stock(
  prices_clean,
  symbols = c("AAPL", "MSFT"),
  column = "Close",
  format = "wide"
)
```

### One panel per symbol

```r
plot_stock(
  prices_clean,
  symbols = c("AAPL", "MSFT"),
  column = "Close",
  format = "long",
  title = "Closing Prices"
)
```

### Limit the date range

```r
plot_stock(
  prices_clean,
  symbols = "AAPL",
  column = "Close",
  start = "2024-01-01",
  end = "2024-06-30"
)
```

At most five symbols are plotted. If more are supplied, only the first five are
used.

## 7. Analyze Autocorrelation

`get_acf()` and `get_pacf()` calculate simple returns before analyzing
autocorrelation:

```text
return = (current value / previous value) - 1
```

They do not calculate ACF or PACF directly from raw price levels.

### ACF

```r
acf_table <- get_acf(
  prices_clean,
  symbols = c("AAPL", "MSFT"),
  column = "Close",
  lag_max = 30
)

head(acf_table)
```

Request the table and chart together:

```r
acf_result <- get_acf(
  prices_clean,
  symbols = "AAPL",
  plot = TRUE
)

acf_result$table
acf_result$plot
```

### PACF

```r
pacf_result <- get_pacf(
  prices_clean,
  symbols = "AAPL",
  column = "Close",
  lag_max = 30,
  plot = TRUE
)

pacf_result$table
pacf_result$plot
```

Both functions support at most five symbols and require at least three valid
observations per series.

## 8. Decompose Seasonality

`plot_seasonal()` uses STL decomposition to display:

- Observed values
- Trend
- Seasonal component
- Remainder

```r
plot_seasonal(
  prices_clean,
  symbol = "AAPL",
  column = "Close"
)
```

The seasonal period is detected automatically from the average date gap:

| Average gap | Period used |
|---|---:|
| Up to 3 days | 5 |
| Up to 10 days | 52 |
| Up to 45 days | 12 |
| Up to 120 days | 4 |
| More than 120 days | 1 |

Override the detected period when needed:

```r
plot_seasonal(
  prices_clean,
  symbol = "AAPL",
  column = "Close",
  period = 5,
  start = "2023-01-01",
  end = "2024-12-31"
)
```

The function requires at least `2 * period` observations. Shorter usable
windows may produce a warning because decomposition quality can be poor.

## 9. Forecast a Stock Series

`forecast_stock()` fits `forecast::auto.arima()` to one symbol and one field.

```r
result <- forecast_stock(
  prices_clean,
  symbol = "AAPL",
  column = "Close",
  horizon = 30,
  ci_levels = c(80, 95)
)
```

The returned list contains:

| Element | Description |
|---|---|
| `model` | Fitted ARIMA model |
| `plot` | Historical data, forecast, and confidence bands |
| `forecast` | Raw `forecast` package result |

Inspect the result:

```r
summary(result$model)
result$plot
result$forecast$mean
result$forecast$lower
result$forecast$upper
```

Limit the fitting window:

```r
result <- forecast_stock(
  prices_clean,
  symbol = "AAPL",
  start = "2022-01-01",
  end = "2024-12-31",
  horizon = 60,
  ci_levels = c(50, 80, 95)
)
```

The function requires at least 10 observations and does not accept missing
values. It forecasts one series at a time. Future dates are generated from the
observed cadence; daily forecasts skip weekends but do not account for exchange
holidays.

## Complete Workflow

```r
library(MISdata)

# 1. Get a reproducible sample of Dow Jones symbols
components <- get_index_components("DJI")
symbols <- sample_symbols(components, n = 3, seed = 42)

# 2. Download daily OHLCV data
prices <- get_stock(
  symbols = symbols,
  start = "2022-01-01",
  end = "2024-12-31",
  columns = c("Open", "High", "Low", "Close", "Volume")
)

# 3. Clean each Symbol/Column series
prices_clean <- clean_stock(prices)

# 4. Plot closing prices
price_plot <- plot_stock(
  prices_clean,
  symbols = symbols,
  column = "Close",
  format = "long"
)
price_plot

# 5. Examine return autocorrelation
acf_result <- get_acf(
  prices_clean,
  symbols = symbols,
  column = "Close",
  lag_max = 30,
  plot = TRUE
)
acf_result$plot

# 6. Aggregate to monthly frequency
monthly <- change_period(prices_clean, period = "monthly")

# 7. Plot monthly seasonality for one stock
seasonal_plot <- plot_seasonal(
  monthly,
  symbol = symbols[1],
  column = "Close",
  period = 12
)
seasonal_plot

# 8. Forecast 12 monthly observations
forecast_result <- forecast_stock(
  monthly,
  symbol = symbols[1],
  column = "Close",
  horizon = 12,
  ci_levels = c(80, 95),
  period = 12
)

summary(forecast_result$model)
forecast_result$plot
```

## Function Reference

| Function | Purpose |
|---|---|
| `get_index_components()` | Retrieve DJI or S&P 500 constituents |
| `sample_symbols()` | Randomly select symbols from a component table |
| `get_stock()` | Download historical stock data |
| `clean_stock()` | Trim and interpolate missing values |
| `change_period()` | Aggregate data to monthly, quarterly, or yearly frequency |
| `convert_stock()` | Convert between supported time-series formats |
| `plot_stock()` | Plot one or more stock series |
| `get_acf()` | Calculate and optionally plot return ACF |
| `get_pacf()` | Calculate and optionally plot return PACF |
| `plot_seasonal()` | Create an STL decomposition plot |
| `forecast_stock()` | Fit an ARIMA model and create forecasts |

Open the built-in R documentation for full argument details:

```r
?MISdata
?get_stock
?clean_stock
?forecast_stock
```

## Troubleshooting

### A symbol cannot be downloaded

Confirm that the ticker is valid for the selected source. Ticker names may
differ by exchange, and delisted securities may not be available.

```r
get_stock("AAPL", start = "2024-01-01", source = "yahoo")
```

### All requested symbols fail

Check the internet connection, ticker names, date range, and upstream service
availability. `get_stock()` stops when no requested symbol returns data.

### Invalid date error

Use ISO date strings:

```r
start = "2024-01-01"
end = "2024-12-31"
```

### Analysis reports missing values

Clean the long data before analysis:

```r
prices_clean <- clean_stock(prices)
sum(is.na(prices_clean$Value))
```

### No data found for a symbol or column

Inspect the values that are actually present:

```r
unique(prices_clean$Symbol)
unique(prices_clean$Column)
```

Symbol and column matching is case-sensitive.

### STL reports insufficient observations

Use a longer date range or a smaller valid `period`. STL requires at least
`2 * period` observations.

### Forecasting reports insufficient observations

Use a longer fitting window. `forecast_stock()` requires at least 10 valid
observations after filtering.

### More than five symbols are supplied

`plot_stock()`, `get_acf()`, and `get_pacf()` use only the first five symbols.
Split larger sets into smaller groups.

## Support and License

- Repository: <https://github.com/MISDataGit/MISdata>
- Issues: <https://github.com/MISDataGit/MISdata/issues>
- License: GPL (>= 3)
