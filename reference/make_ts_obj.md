# Set up multiplicative time-series patterns

Store supplied seasonal, trend, and shock functions with empty parameter
lists, or construct them from their parameter lists.

## Usage

``` r
make_ts_obj(obj, options)
```

## Arguments

- obj:

  the object receiving the pattern functions and parameters

- options:

  a named list of resolved function and parameter values: `F_season`,
  `F_trend`, `F_shock` and their `season_par`, `trend_par`, `shock_par`
  counterparts. Human population patterns may instead use `H_season`,
  `H_trend`, `H_shock` and matching `H_*_par` names.

## Value

the updated object
