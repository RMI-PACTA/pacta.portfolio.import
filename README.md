# pacta.portfolio.import <a href="https://rmi-pacta.github.io/pacta.portfolio.import"><img src="man/figures/logo.png" align="right" height="31" /></a>

<!-- badges: start -->
[![Lifecycle: stable](https://img.shields.io/badge/lifecycle-stable-brightgreen.svg)](https://lifecycle.r-lib.org/articles/stages.html#stable)
[![R-CMD-check](https://github.com/RMI-PACTA/pacta.portfolio.import/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/RMI-PACTA/pacta.portfolio.import/actions/workflows/R-CMD-check.yaml)
[![codecov](https://img.shields.io/codecov/c/github/rmi-pacta/pacta.portfolio.import)](https://codecov.io/gh/RMI-PACTA/pacta.portfolio.import)
[![CRAN status](https://www.r-pkg.org/badges/version/pacta.portfolio.import)](https://CRAN.R-project.org/package=pacta.portfolio.import)
[![pacta.portfolio.import status badge](https://rmi-pacta.r-universe.dev/badges/pacta.portfolio.import)](https://rmi-pacta.r-universe.dev/pacta.portfolio.import)
<!-- badges: end -->

The `pacta.portfolio.import` R package provides a number of functions to facilitate the importing of a portfolio CSV intended to be used by the [`pacta.portfolio.audit`](https://github.com/RMI-PACTA/pacta.portfolio.audit) and [`pacta.portfolio.allocate`](https://github.com/RMI-PACTA/pacta.portfolio.allocate) R packages. Its primary exported function is `read_portfolio_csv()` which can read in one or more portfolio CSVs. Its other utility functions allow `read_portfolio_csv()` to work around a variety of formatting and encoding issues that have been seen in the wild, such as: automatically determining strange file encodings, strange number formats and numerical marks, and adapting to non-standard delimiters.

## Installation

You can install the development version of pacta.portfolio.import from
[R-universe](https://r-universe.dev/) with:

``` r
install.packages("pacta.portfolio.import", repos = "https://rmi-pacta.r-universe.dev")
```

You can install the development version of pacta.portfolio.import from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("RMI-PACTA/pacta.portfolio.import")
```

## Usage

To read in portfolio CSVs, you can pass a vector of relative or absolute file paths to `read_portfolio_csv()`. You can also pass the path to a directory and `read_portfolio_csv()` will import all readable CSVs found in the directory. For example...

``` r
library("pacta.portfolio.import")

# read in a single portfolio CSV
read_portfolio_csv("portfolio_1.csv")

# read in multiple portfolio CSVs
read_portfolio_csv(c("portfolio_1.csv", "portfolio_2.csv"))

# read in all portfolio CSVs in a directory
read_portfolio_csv("portfolios/")
```

