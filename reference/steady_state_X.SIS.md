# Compute steady states for `SIS` (**XH**)

Compute steady-state infection densities for the `SIS` module given
daily FoI `foi` and fixed host population density `H`, with no mortality
and no mass-treatment events.

## Usage

``` r
# S3 method for class 'SIS'
steady_state_X(foi, H, xds_obj, i = 1)
```

## Arguments

- foi:

  the daily FoI

- H:

  human / host population density

- xds_obj:

  an **`xds`** model object

- i:

  the host species index

## Value

a named [list](https://rdrr.io/r/base/list.html) with elements `S` and
`I`

## See also

[SIS](https://dd-harp.github.io/ramp.xds/reference/SIS.md)
