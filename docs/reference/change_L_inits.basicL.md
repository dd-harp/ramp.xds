# Change initial values for `basicL` (**L**)

Change initial values named in `options` for the `basicL` module,
leaving other values unchanged.

Initial values of the variable \\(L\\) are reset if they are passed as a
named component of `options`.

## Usage

``` r
# S3 method for class 'basicL'
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

Stored initial values for this module are at `xds_obj$L_obj[[s]]$inits`.

## See also

[make_L_inits_basicL](https://dd-harp.github.io/ramp.xds/reference/make_L_inits_basicL.md),
[setup_L_inits.basicL](https://dd-harp.github.io/ramp.xds/reference/setup_L_inits.basicL.md)
