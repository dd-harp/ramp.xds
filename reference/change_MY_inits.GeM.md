# Change initial values for `GeM` (**MY**)

Change initial values named in `options` for the `GeM` module, leaving
other values unchanged.

## Usage

``` r
# S3 method for class 'GeM'
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

  density of parous adult mosquitoes

- `Y`:

  density of infected adult mosquitoes

- `Z`:

  density of infectious adult mosquitoes

## See also

[make_MY_inits_GeM](https://dd-harp.github.io/ramp.xds/reference/make_MY_inits_GeM.md),
[setup_MY_inits.GeM](https://dd-harp.github.io/ramp.xds/reference/setup_MY_inits.GeM.md)
