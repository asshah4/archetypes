
<!-- README.md is generated from README.Rmd. Please edit that file -->

# rx

<!-- badges: start -->

[![Codecov test
coverage](https://codecov.io/gh/asshah4/rx/branch/main/graph/badge.svg)](https://codecov.io/gh/asshah4/rx?branch=main)
[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
[![R-CMD-check](https://github.com/asshah4/rx/workflows/R-CMD-check/badge.svg)](https://github.com/asshah4/rx/actions)
[![CRAN
status](https://www.r-pkg.org/badges/version/rx)](https://CRAN.R-project.org/package=rx)
<!-- badges: end -->

The goal of `rx` is to prescribe `formula` objects with additional
attributes.

## Installation

You can install the released version of rx from
[CRAN](https://CRAN.R-project.org) with:

``` r
#install.packages("rx")
```

And the development version from [GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("asshah4/rx")
```

## Usage

The `rx` package is simple, and only has several major functions.

-   `rx()`
-   `set_rx_labels()`

It simply adds a layer of attributes to `formula` objects that can help
simplify their usage in other packages, particularly in the field of
epidemiology and causality-based modeling.

``` r
library(rx)
# Create a simple formula
f <- mpg ~ wt + hp + cyl
new_f <- rx(f)
class(new_f)
#> [1] "rx"      "formula"
# Identifies the roles of individual terms
term_tbl <- attributes(new_f)$roles

# These can also be listed out as labels if need be
table_to_list(term_tbl)
#> $lhs
#> [1] "mpg"
#> 
#> $rhs
#> [1] "wt"  "hp"  "cyl"
```

For other purposes, the individual terms may need to be labeled. For
example, in the `murmur` package, exposures and fixed variables are
treated differently.

``` r
# This can be done manually
set_rx_labels(labels = list(
    X = "exposures",
    F = "fixed"
))

f <- mpg ~ X(wt) + hp + F(cyl)
new_f <- rx(f)

# Identifies the roles of individual terms
attributes(new_f)$roles %>%
    table_to_list()
#> $exposures
#> [1] "wt"
#> 
#> $fixed
#> [1] "cyl"
#> 
#> $lhs
#> [1] "mpg"
#> 
#> $rhs
#> [1] "wt"  "hp"  "cyl"
# This can also be done through "themes" based on individual packages
reset_rx_labels()
set_rx_theme("murmur")
f <- mpg ~ X(wt) + hp + F(cyl)
new_f <- rx(f)
attributes(new_f)$roles %>%
    table_to_list()
#> $exposures
#> [1] "wt"
#> 
#> $fixed
#> [1] "cyl"
#> 
#> $outcomes
#> [1] "mpg"
#> 
#> $predictors
#> [1] "wt"  "hp"  "cyl"
```
