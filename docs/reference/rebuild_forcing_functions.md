# Rebuild Forcing Functions

Rebuild forcing functions using `make_function`:

- `F_season` is made from `season_par`

- `F_trend` is made from `trend_par`

- `F_shock` is made from `shock_par`

## Usage

``` r
rebuild_forcing_functions(xds_obj, ix)
```

## Arguments

- xds_obj:

  an **`xds`** model object

- ix:

  the species index

## Value

an **`xds`** model object

## See also

[`readXDS()`](https://dd-harp.github.io/ramp.xds/reference/readXDS.md)
