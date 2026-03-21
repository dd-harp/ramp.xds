# Set **L** Component parameters for `trivial`

If `Lambda`, `season_par`, `trend_par`, or `shock_par` are named in
`options`, the old value is replaced. After updating the parameter
objects, `F_season`, `F_trend`, and `F_shock` are recompiled by calling
[make_function](https://dd-harp.github.io/ramp.xds/reference/make_function.md)
on the updated parameters via
[rebuild_forcing_functions](https://dd-harp.github.io/ramp.xds/reference/rebuild_forcing_functions.md).

## Usage

``` r
# S3 method for class 'trivial'
change_L_pars(xds_obj, s = 1, options = list())
```

## Arguments

- xds_obj:

  an **`xds`** model object

- s:

  the vector species index

- options:

  a named list

## Value

an **`xds`** object
