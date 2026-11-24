# Change initial values for `RMdts` (**MY**)

Change initial values named in `options` for the `RMdts` module, leaving
other values unchanged. Rebuild initial values using
[make_MY_inits_RMdts](https://dd-harp.github.io/ramp.xds/reference/make_MY_inits_RMdts.md)
and the selected species' `max_eip`; scalar values are expanded to the
required lengths.

## Usage

``` r
# S3 method for class 'RMdts'
change_MY_inits(xds_obj, options = list(), s = 1)
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

Stored initial values for this module are at
`xds_obj$MY_obj[[s]]$inits`.

## See also

[make_MY_inits_RMdts](https://dd-harp.github.io/ramp.xds/reference/make_MY_inits_RMdts.md),
[setup_MY_inits.RMdts](https://dd-harp.github.io/ramp.xds/reference/setup_MY_inits.RMdts.md)
