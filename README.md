
<!-- README.md is generated from README.Rmd. Please edit that file -->

# axe

<!-- badges: start -->

[![Codecov test
coverage](https://codecov.io/gh/asshah4/axe/branch/main/graph/badge.svg)](https://codecov.io/gh/asshah4/axe?branch=main)
[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
[![R-CMD-check](https://github.com/asshah4/axe/workflows/R-CMD-check/badge.svg)](https://github.com/asshah4/axe/actions)
[![CRAN
status](https://www.r-pkg.org/badges/version/axe)](https://CRAN.R-project.org/package=axe)
<!-- badges: end -->

*Formulas with prescribed operations* are described within the `{axe}`
package, which helps to extend and vectorize the base R `formula` class.
The purpose is to help **chop** a formula-based object into underlying
`term` objects, and **rebuild** it as *prescribed formula*.

## Installation

You can install the released version of `axe` from
[CRAN](https://CRAN.R-project.org) with:

``` r
#install.packages("axe")
```

And the development version from [GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("asshah4/axe")
```

## Usage

The `axe` package is simple, and only has several major functions.
