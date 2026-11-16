# Change initial values for `SI` (**MY**)

For the `SI` module (**MY**) for the \\s^{th}\\ species, change initial
values named in `options` leaving other values unchanged.

## Usage

``` r
# S3 method for class 'SI'
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

an **`xds`** object

## State Variables

- `M`:

  density of adult mosquitoes

- `P`:

  density of parous adult mosquitoes

- `Y`:

  density of infected adult mosquitoes

## See also

[SI](https://dd-harp.github.io/ramp.xds/reference/SI.md),
[get_MY_inits](https://dd-harp.github.io/ramp.xds/reference/get_MY_inits.md),
[make_MY_inits_SI](https://dd-harp.github.io/ramp.xds/reference/make_MY_inits_SI.md),
[setup_MY_inits.SI](https://dd-harp.github.io/ramp.xds/reference/setup_MY_inits.SI.md)
