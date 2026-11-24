# Change initial values for `basicM` (**MY**)

Change initial values named in `options` for the `basicM` module,
leaving other values unchanged.

## Usage

``` r
# S3 method for class 'basicM'
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

## State Variables

- `M`:

  density of adult mosquitoes

- `P`:

  density of parous adult mosquitoes: those that have blood-fed at least
  once

## See also

[make_MY_inits_basicM](https://dd-harp.github.io/ramp.xds/reference/make_MY_inits_basicM.md),
[setup_MY_inits.basicM](https://dd-harp.github.io/ramp.xds/reference/setup_MY_inits.basicM.md)
