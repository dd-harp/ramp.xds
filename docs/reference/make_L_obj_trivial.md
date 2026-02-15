# Make `L_obj` for `trivial` (**L** Component)

The number of emerging adults is a function \$\$\Lambda S(t) T(t)\$\$
where

- \\\Lambda\\ or `Lambda` is the mean number of adult female mosquitoes
  emerging per day

- \\S(t)\\ or `F_season` is a seasonal signal (ideally, with an average
  annual mean of 1)

- \\T(t)\\ or `F_trend` is a function returning a trend (ideally, with
  an average value of 1)

- \\P(t)\\ or `F_shock` is a function returning a perturbation (by
  default, set to 1)

## Usage

``` r
make_L_obj_trivial(
  nHabitats,
  options = list(),
  Lambda = 1000,
  season_par = makepar_F_one(),
  trend_par = makepar_F_one(),
  shock_par = makepar_F_one()
)
```

## Arguments

- nHabitats:

  the number of habitats in the model

- options:

  a [list](https://rdrr.io/r/base/list.html) that overwrites default
  values

- Lambda:

  vector of mean emergence rates from each aquatic habitat

- season_par:

  an object to configure a seasonality function using
  [make_function](https://dd-harp.github.io/ramp.xds/reference/make_function.md)

- trend_par:

  an object to configure a trends function using
  [make_function](https://dd-harp.github.io/ramp.xds/reference/make_function.md)

- shock_par:

  an object to configure a shocks function using
  [make_function](https://dd-harp.github.io/ramp.xds/reference/make_function.md)

## Value

none
