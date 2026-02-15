# Make parameters for trivial aquatic mosquito model

Make parameters for trivial aquatic mosquito model

## Usage

``` r
make_MY_obj_trivial(
  nPatches,
  options,
  f = 1,
  q = 1,
  Z = 1,
  eggs = 1,
  season_par = makepar_F_one(),
  trend_par = makepar_F_one(),
  shock_par = makepar_F_one()
)
```

## Arguments

- nPatches:

  an integer

- options:

  a [list](https://rdrr.io/r/base/list.html) of values that overwrites
  the defaults

- f:

  the blood feeding rate

- q:

  the human fraction

- Z:

  the human fraction

- eggs:

  the human fraction

- season_par:

  parameters to configure a `F_season` using
  [make_function](https://dd-harp.github.io/ramp.xds/reference/make_function.md)

- trend_par:

  parameters to configure `F_trend` using
  [make_function](https://dd-harp.github.io/ramp.xds/reference/make_function.md)

- shock_par:

  parameters to configure `F_shock` using
  [make_function](https://dd-harp.github.io/ramp.xds/reference/make_function.md)

## Value

none
