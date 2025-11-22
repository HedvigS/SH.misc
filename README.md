
<!-- README.md is generated from README.Rmd. Please edit that file -->

# SH.misc

<!-- badges: start -->

<!-- badges: end -->

This is a repos for functions often used by Siva Kalyan and Hedvig
Skirgård. They mainly concern linguistic data manipulation and related
convenient functions to academic research.

## IN DEVELOPMENT / WORK IN PROGRESS / PACKAGE VERSIONING

This R-package is not ready for full release yet, it’s under
development. When it is ready, we will release the first version and
then you can refer to package version numbers.

## Installation

You can install the development version of SH.misc from
[GitHub](https://github.com/) with:

``` r
library(remotes)
remotes::install_github("HedvigS/SH.misc")

library(SH.misc)
```

## Example

This is a basic example which shows you how to solve a common problem:

``` r
library(SH.misc)
#> Warning: replacing previous import 'data.table::first' by 'dplyr::first' when
#> loading 'SH.misc'
#> Warning: replacing previous import 'ape::where' by 'dplyr::where' when loading
#> 'SH.misc'
#> Warning: replacing previous import 'data.table::last' by 'dplyr::last' when
#> loading 'SH.misc'
#> Warning: replacing previous import 'data.table::between' by 'dplyr::between'
#> when loading 'SH.misc'
#> Warning: replacing previous import 'magrittr::set_names' by 'purrr::set_names'
#> when loading 'SH.misc'
#> Warning: replacing previous import 'data.table::transpose' by
#> 'purrr::transpose' when loading 'SH.misc'
#> Warning: replacing previous import 'magrittr::extract' by 'tidyr::extract' when
#> loading 'SH.misc'
## basic example code
```

What is special about using `README.Rmd` instead of just `README.md`?
You can include R chunks like so:

``` r
summary(cars)
#>      speed           dist       
#>  Min.   : 4.0   Min.   :  2.00  
#>  1st Qu.:12.0   1st Qu.: 26.00  
#>  Median :15.0   Median : 36.00  
#>  Mean   :15.4   Mean   : 42.98  
#>  3rd Qu.:19.0   3rd Qu.: 56.00  
#>  Max.   :25.0   Max.   :120.00
```

You’ll still need to render `README.Rmd` regularly, to keep `README.md`
up-to-date. `devtools::build_readme()` is handy for this.

You can also embed plots, for example:

<img src="man/figures/README-pressure-1.png" width="100%" />

In that case, don’t forget to commit and push the resulting figure
files, so they display on GitHub and CRAN.
