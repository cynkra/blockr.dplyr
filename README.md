
<!-- README.md is generated from README.Rmd. Please edit that file -->

# Data transformation for `blockr.core`

<!-- badges: start -->

[![check](https://github.com/cynkra/blockr.dplyr/actions/workflows/check.yaml/badge.svg)](https://github.com/cynkra/blockr.dplyr/actions/workflows/check.yaml)
<!-- badges: end -->

Extending `blockr.core` with blocks for data wrangling, `blockr.dplyr`
wraps several `dplyr` verbs.

## Installation

You can install the development version of blockr.dplyr from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("cynkra/blockr.dplyr")
```

## Example

A `dplyr::select()` block can be spun up as

``` r
library(blockr.dplyr)
blockr.core::serve(new_select_block(c("mpg", "cyl")), list(data = mtcars))
```

A `dplyr::mutate()` block can be used to add new variables:

``` r
library(blockr.dplyr)
blockr.core::serve(new_mutate_block(), list(data = mtcars))
```

And a two-table verb such as `dplyr::join()` is available as

``` r
library(blockr.dplyr)
blockr.core::serve(
  new_join_block(by = "name"),
  data = list(x = dplyr::band_members, y = dplyr::band_instruments)
)
```
