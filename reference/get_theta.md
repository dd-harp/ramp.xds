# Get the slope of density-dependent mortality (**L**)

Return the stored realized slope of density-dependent mortality per
habitat.

## Usage

``` r
get_theta(xds_obj, s = 1)
```

## Arguments

- xds_obj:

  an **`xds`** model object

- s:

  the vector species index

## Value

a [numeric](https://rdrr.io/r/base/numeric.html) vector of length
`nHabitats`

## Note

Dispatch on the module object at `xds_obj$L_obj[[s]]`. This function
does not evaluate ports or apply effect sizes.

## See also

[change_theta](https://dd-harp.github.io/ramp.xds/reference/change_theta.md)
