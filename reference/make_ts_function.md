# Make a Time Series Function

Build a function that generates a time series with known functions. The
value of variable \\x(t)\\ is computed as a product of four configurable
elements:

- \\\bar x\\: a mean value

- \\S(t)\\: a seasonal pattern

- \\T(t)\\: a trend

- \\K(t)\\: a shock

\$\$x(t) = \bar x \times S(t) \times T(t) \times K(t)\$\$

The component functions are specified by passing parameters for
[make_function](https://dd-harp.github.io/ramp.xds/reference/make_function.md):

- `season_par` creates \\S(t)\\ or `F_season` (*eg,* using
  [makepar_F_sin](https://dd-harp.github.io/ramp.xds/reference/makepar_F_sin.md))

- `trend_par` creates \\T(t)\\ or `F_trend` (*eg,* using
  [makepar_F_spline](https://dd-harp.github.io/ramp.xds/reference/makepar_F_spline.md))

- `shock_par` creates \\K(t)\\ or `F_shock` (*eg,* using
  [makepar_F_sharkbite](https://dd-harp.github.io/ramp.xds/reference/makepar_F_sharkbite.md))

## Usage

``` r
make_ts_function(
  options = list(),
  N = 1,
  scale = 1,
  season_par = list(),
  trend_par = list(),
  shock_par = list()
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

  seasonality function for

- trend_par:

  trend function parameters

- shock_par:

  trend function parameters

## Value

a function
