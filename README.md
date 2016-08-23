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
## # A tibble: 67,733 x 5
##                  location   country report_date report_type value
##                     <chr>     <chr>      <date>       <chr> <dbl>
## 1  Argentina-Buenos_Aires Argentina  2016-03-19   confirmed     2
## 2  Argentina-Buenos_Aires Argentina  2016-03-26   confirmed     2
## 3  Argentina-Buenos_Aires Argentina  2016-04-02   confirmed     2
## 4  Argentina-Buenos_Aires Argentina  2016-04-09   confirmed     2
## 5  Argentina-Buenos_Aires Argentina  2016-04-16   confirmed     2
## 6  Argentina-Buenos_Aires Argentina  2016-04-29   confirmed     2
## 7  Argentina-Buenos_Aires Argentina  2016-05-07   confirmed     4
## 8  Argentina-Buenos_Aires Argentina  2016-05-14   confirmed     2
## 9  Argentina-Buenos_Aires Argentina  2016-05-22   confirmed     4
## 10 Argentina-Buenos_Aires Argentina  2016-05-30   confirmed     2
## # ... with 67,723 more rows
```

You can explore that data with `explore()`, which will open a shiny app.
