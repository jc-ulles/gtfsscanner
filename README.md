
<!-- README.md is generated from README.Rmd. Please edit that file -->

# gtfsscanner

<!-- badges: start -->

[![R-CMD-check](https://github.com/jc-ulles/gtfsscanner/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/jc-ulles/gtfsscanner/actions/workflows/R-CMD-check.yaml)

<!-- badges: end -->

Determines the geographical position of vehicles according to a
timetable, based on GTFS data.

## Installation

You can install the development version of gtfsscanner from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("jc-ulles/gtfsscanner")
```

## Example

This is a basic example which shows you how to solve a common problem:

``` r
library(gtfsscanner)

gtfs_scan(dir = "C/.../GTFS.zip",
          day = "2025-11-05",
          time = "08:05:00")
```
