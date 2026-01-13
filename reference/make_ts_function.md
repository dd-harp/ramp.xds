# Make a Time Series Function

Make a Time Series Function

## Usage

``` r
make_ts_function(
  options = list(),
  N = 1,
  scale = 1,
  season_par = list(),
  trend_par = list()
)
```

## Arguments

- options:

  configurable options

- N:

  the length of the return value

- scale:

  scale parameter, usually the average

- season_par:

  seasonality function parameters

- trend_par:

  trend function parameters

## Value

a function
