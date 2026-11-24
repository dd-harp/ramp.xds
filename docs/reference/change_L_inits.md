# Change initial values (**L**)

Change initial values named in `options` for the selected **L** module
and vector species, leaving other values unchanged.

## Usage

``` r
change_L_inits(xds_obj, options = list(), s = 1)
```

## Arguments

- xds_obj:

  an **`xds`** model object

- options:

  a named list

- s:

  the vector species index

## Value

an **`xds`** model object

## Note

Dispatch uses the class of `xds_obj$L_obj[[s]]`.

## See also

[get_L_inits](https://dd-harp.github.io/ramp.xds/reference/get_L_inits.md),
[setup_L_inits](https://dd-harp.github.io/ramp.xds/reference/setup_L_inits.md),
[xds_initial_conditions](https://dd-harp.github.io/ramp.xds/reference/xds_initial_conditions.md)
