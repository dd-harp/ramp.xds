# Change parameters for `trivial` (**L**)

For the `trivial` module (**L**) for the \\s^{th}\\ species, change
parameter values named in `options` leaving other values unchanged.

Parameters: `Lambda`, `F_season`, `F_trend`, or `F_shock`.

## Usage

``` r
# S3 method for class 'trivial'
change_L_pars(xds_obj, options = list(), s = 1)
```

## Arguments

- xds_obj:

  an **`xds`** model object

- options:

  a named list

- s:

  the vector species index

## Value

an **`xds`** object

## Note

Utilities for trace functions are in `ramp.func`
