
<!-- README.md is generated from README.Rmd. Please edit that file -->

# eventful

<!-- badges: start -->
<!-- badges: end -->

The goal of `eventful` is to streamline the process of running
single-firm rolling-window event studies. The package is built around
the key function `rolling_event_study`. This function takes as input a
formula, a dataframe, and several additional options (width of rolling
window, dates to exclude in estimation, dates for which to generate
dummy variables, and more). It returns a nested dataframe structure with
the key results and preserve input options, so that downstream function
can be called directly on this result without re-supplying these
options.

## Installation

You can install the development version of `eventful` from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("moneyhun/eventful")
```

## Example

See below for an example on simulated data.

1.  Simulate stock return data

``` r
library(eventful)
set.seed(1)
disc_dates    <- lubridate::ymd(c('2020-12-01', '2020-12-31'))
misrep_dates  <- lubridate::ymd(c('2020-01-01'), '2020-02-01')
exclude_dates <- unique(c(disc_dates, misrep_dates))

simdata <- data.frame(
  date     = seq.Date(lubridate::ymd('2019-01-01'),
                      lubridate::ymd('2020-12-31'),
                      by = 'day'),
  aaa_ret  = rnorm(731, .0012, .01),
  bbb_ret  = rnorm(731, .0014, .02),
  ccc_ret  = rnorm(731, .0015, .03)
)

simdata$ddd_ret <- 
  (.33*simdata$aaa_ret + .33*simdata$bbb_ret + .33*simdata$ccc_ret) + 
  rnorm(731, -0.005, sd = 0.01)

simdata$ddd_ret <- 
  ifelse(simdata$date %in% disc_dates, simdata$ddd_ret - .05, simdata$ddd_ret)
```

2.  Run event study model

``` r
event_study <- rolling_event_study(
  formula = ddd_ret ~ aaa_ret + bbb_ret + ccc_ret,
  data = simdata,
  pred_date_range = lubridate::ymd(c('2020-01-01', '2020-12-31')),
  exclude_dates = unique(c(misrep_dates, disc_dates)),
  misrep_dates = misrep_dates,
  disc_dates = disc_dates,
  rolling_window = 50,
  orth = ccc_ret ~ aaa_ret + bbb_ret,
  roll_fixed = 'window'
)
```

3.  Generate residual plot

``` r
labels <- c(
    'aaa_ret' = 'AAA', 
    'bbb_ret' = 'BBB', 
    'ccc_ret' = 'CCC', 
    'ddd_ret' = 'DDD'
  )

(resid_plot <- make_resid_plot(
  event_study, 
  include_labs = TRUE
))
```

<img src="README_files/figure-gfm/Generate residual plot-1.png" width="100%" />
4. Generate coefficient plot, exluding labels.

``` r
(coef_plot <- make_coef_plot(
  event_study, 
  include_labs = FALSE
))
```

<img src="README_files/figure-gfm/Generate coefficient plot-1.png" width="100%" />
