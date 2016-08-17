# zikar

An R package for exploring publicly available Zika data. 
Currently it provides easy access to data in [this repository](https://github.com/cdcepi/zika).

## Installation

This package is not on CRAN, but you can install via devtools with:


```r
devtools::install_github("cpsievert/zikar")
```

## Getting Started

Currently there is really only one dataset of interest


```r
library(zikar)
zika
```

```
## # A tibble: 132,710 x 10
##    report_date               location   country    locationA locationB
##         <date>                  <chr>     <chr>        <chr>     <chr>
## 1   2016-03-19 Argentina-Buenos_Aires Argentina Buenos_Aires      <NA>
## 2   2016-03-19 Argentina-Buenos_Aires Argentina Buenos_Aires      <NA>
## 3   2016-03-19 Argentina-Buenos_Aires Argentina Buenos_Aires      <NA>
## 4   2016-03-19 Argentina-Buenos_Aires Argentina Buenos_Aires      <NA>
## 5   2016-03-19 Argentina-Buenos_Aires Argentina Buenos_Aires      <NA>
## 6   2016-03-19 Argentina-Buenos_Aires Argentina Buenos_Aires      <NA>
## 7   2016-03-19         Argentina-CABA Argentina         CABA      <NA>
## 8   2016-03-19         Argentina-CABA Argentina         CABA      <NA>
## 9   2016-03-19         Argentina-CABA Argentina         CABA      <NA>
## 10  2016-03-19         Argentina-CABA Argentina         CABA      <NA>
## # ... with 132,700 more rows, and 5 more variables: location_type <chr>,
## #   data_field <chr>, data_field_code <chr>, value <dbl>, confirmed <chr>
```

You can explore that data with `explore()`, which will open a shiny app.
