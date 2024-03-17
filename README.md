
<!-- README.md is generated from README.Rmd. Please edit that file -->

# destup

<!-- badges: start -->
<!-- badges: end -->

> A toolkit to convert ‘DeST’ models to ‘EnergyPlus’ models

## Installation

You can install the development version of destup like so:

``` r
install.packages("destep",
    repos = c(
        hongyuanjia = "https://hongyuanjia.r-universe.dev",
        cran = "https://cran.r-project.org"
    )
)
```

## Supported DeST components

This package is still under heavy development and is not ready for use.
Currently, the following components are supported:

- [x] Geometry
- [x] Materials
- [x] Construction
- [ ] Internal gains \[WIP\]
- [ ] HVAC

``` r
library(destup)
## basic example code
```
