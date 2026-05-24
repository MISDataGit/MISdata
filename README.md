# MISdata

End-to-end pipeline for stock market data analysis in R.

## Overview

`MISdata` provides a complete workflow for retrieving, cleaning,
transforming, and analyzing stock market data. The package fetches
index constituents from Wikipedia and OHLCV history from Yahoo Finance,
then offers utilities for NA handling, time-series format conversion
(via `tsbox`), period aggregation with OHLCV-aware semantics,
exploratory analysis (line plots, ACF/PACF, STL decomposition), and
ARIMA-based forecasting with multi-level confidence bands.

This package was developed as part of an undergraduate project.
It is not intended for CRAN submission but follows CRAN-compatible
standards.

## Installation

Install the latest stable version from GitHub:

    # install.packages("remotes")
    remotes::install_github("MISDataGit/MISdata")

## Quick Example

A typical end-to-end workflow:

    library(MISdata)

    # 1. Fetch index components from Wikipedia
    dji  <- get_index_components("DJI")
    syms <- sample_symbols(dji, n = 3, seed = 42)

    # 2. Download OHLCV data from Yahoo Finance
    prices <- get_stock(syms, start = "2023-01-01", end = "2024-12-31")

    # 3. Clean missing values
    prices_clean <- clean_stock(prices, na_method = "trim")

    # 4. Convert to wide xts (or tsibble, zoo, ts)
    prices_xts <- convert_stock(prices_clean, to = "xts")

    # 5. Aggregate to monthly frequency (OHLCV-aware)
    monthly <- change_period(prices_clean, period = "monthly")

    # 6. Exploratory plots
    plot_stock(prices_clean, format = "long")
    get_acf(prices_clean, symbols = syms[1], plot = TRUE)
    plot_seasonal(monthly, symbol = syms[1])

    # 7. Forecast with auto.arima
    fc <- forecast_stock(prices_clean, symbol = syms[1], horizon = 30)
    summary(fc$model)
    fc$plot

## Function Reference

The package is organized into six modules:

| Module | Functions | Purpose |
|---|---|---|
| `fetch.R`    | `get_index_components()`, `sample_symbols()`, `get_stock()` | Retrieve index constituents and OHLCV data |
| `clean.R`    | `clean_stock()` | Group-wise NA handling on long data frames |
| `convert.R`  | `convert_stock()` | Convert between long df, xts, tsibble, zoo, ts |
| `period.R`   | `change_period()` | OHLCV-aware period aggregation |
| `eda.R`      | `plot_stock()`, `get_acf()`, `get_pacf()`, `plot_seasonal()` | Visualization and autocorrelation analysis |
| `forecast.R` | `forecast_stock()` | ARIMA forecasting via `forecast::auto.arima()` |

For full documentation, install the package and run `?MISdata` or
`?<function_name>`.

## Authors

- Batuhan Karaköy
- Enes Türkoğlu
- Metehan Kaygısız

## License

GPL (>= 3). See [LICENSE.md](LICENSE.md) for the full text.
