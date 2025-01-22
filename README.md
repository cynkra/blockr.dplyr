
<!-- README.md is generated from README.Rmd. Please edit that file -->

# Data transformation for `blockr.core`

<!-- badges: start -->

[![check](https://github.com/cynkra/blockr.dplyr/actions/workflows/check.yaml/badge.svg)](https://github.com/cynkra/blockr.dplyr/actions/workflows/check.yaml)
<!-- badges: end -->

Extending `blockr.core` with blocks for data wrangling, `blockr.dplyr`
wraps several `dplyr` verbs.

The package provides four transformation blocks:

- `new_select_block()`: Subset columns in a data frame using
  `dplyr::select()`. Allows interactive column selection through a
  dropdown menu.
- `new_mutate_block()`: Add or modify columns in a data frame using
  `dplyr::mutate()`. Features a key-value editor with autocompletion and
  expression mode.
- `new_arrange_block()`: Order the rows of a data frame by column values
  using `dplyr::arrange()`. Supports both ascending and descending
  order.
- `new_join_block()`: Join two data frames using `dplyr::*_join()`.
  Supports all dplyr join types (left, right, inner, full, semi, anti)
  with an interactive interface for specifying join columns.

## Installation

You can install the development version of blockr.dplyr from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("cynkra/blockr.dplyr")
```

## Examples

``` r
library(blockr.dplyr)
pkgload::load_all(); blockr.core::serve(new_select_block(c("mpg", "cyl")), list(data = mtcars))
pkgload::load_all(); blockr.core::serve(new_mutate_block(), list(data = mtcars))
pkgload::load_all(); blockr.core::serve(new_arrange_block(c("mpg", "cyl")), list(data = mtcars))
pkgload::load_all(); blockr.core::serve(new_join_block(by = "name"), list(x = dplyr::band_members, y = dplyr::band_instruments))
```
