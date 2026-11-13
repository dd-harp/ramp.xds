# Setup initial values for `SIS` (**XH**)

Set initial values for all the variables in the `SIS` module (**XH**)
for the \\i^{th}\\ species.

- [make_XH_inits_SIS](https://dd-harp.github.io/ramp.xds/reference/make_XH_inits_SIS.md)
  supplies default values

- The values in `options` override the defaults

- To inspect variable names, use
  [get_XH_inits](https://dd-harp.github.io/ramp.xds/reference/get_XH_inits.md)
  or see the inline module documentation.

## Usage

``` r
# S3 method for class 'SIS'
setup_XH_inits(xds_obj, options = list(), i = 1)
```

## Arguments

- xds_obj:

  an **`xds`** model object

- options:

  a [list](https://rdrr.io/r/base/list.html)

- i:

  the host species index

## Value

an **`xds`** object

## State Variables

- `H`:

  the total human or host population density

- `I`:

  the density of infected humans or hosts

- `S=H-I`:

  the density of susceptible humans or hosts
