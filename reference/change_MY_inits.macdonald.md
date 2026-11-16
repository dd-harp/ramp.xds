# Change initial values for `macdonald` (**MY**)

For the `macdonald` module (**MY**) for the \\s^{th}\\ species, change
initial values named in `options` leaving other values unchanged.

## Usage

``` r
# S3 method for class 'macdonald'
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

- `Z`:

  density of infectious adult mosquitoes

## See also

[macdonald](https://dd-harp.github.io/ramp.xds/reference/macdonald.md),
[get_MY_inits](https://dd-harp.github.io/ramp.xds/reference/get_MY_inits.md),
[make_MY_inits_macdonald](https://dd-harp.github.io/ramp.xds/reference/make_MY_inits_macdonald.md),
[setup_MY_inits.macdonald](https://dd-harp.github.io/ramp.xds/reference/setup_MY_inits.macdonald.md)
