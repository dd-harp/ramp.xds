# Make parameters for trivial human model

The trivial module configures forcing with a seasonal pattern

- \\H = F_H(t)\\ - human / host population density

- \\I = F_I(t)\\ - human / host infectious density

Since \\\kappa\\ is the ratio \\I/H\\

## Usage

``` r
make_XH_obj_trivial(
  nPatches,
  options,
  kappa = 0.1,
  HPop = 1,
  season_par = makepar_F_one(),
  trend_par = makepar_F_one(),
  shock_par = makepar_F_one(),
  H_trend_par = makepar_F_one()
)
```

## Arguments

- nPatches:

  the number of patches

- options:

  a [list](https://rdrr.io/r/base/list.html)

- kappa:

  net infectiousness

- HPop:

  initial human population density

- season_par:

  parameters to configure a `F_season` using
  [make_function](https://dd-harp.github.io/ramp.xds/reference/make_function.md)

- trend_par:

  parameters to configure `F_trend` using
  [make_function](https://dd-harp.github.io/ramp.xds/reference/make_function.md)

- shock_par:

  parameters to configure `F_shock` using
  [make_function](https://dd-harp.github.io/ramp.xds/reference/make_function.md)

- H_trend_par:

  parameters to configure `H_trend` using
  [make_function](https://dd-harp.github.io/ramp.xds/reference/make_function.md)

## Value

a [list](https://rdrr.io/r/base/list.html)
