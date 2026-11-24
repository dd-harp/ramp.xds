# Get the initial adult mosquito density (**MY**)

Return the initial adult mosquito density per patch.

## Usage

``` r
get_M(xds_obj, s = 1)
```

## Arguments

- xds_obj:

  an **`xds`** model object

- s:

  the vector species index

## Value

a [numeric](https://rdrr.io/r/base/numeric.html) vector of length
`nPatches`

## Details

Return initial density, not current solver states or output
trajectories.

## Note

Dispatch on the module object at `xds_obj$MY_obj[[s]]`.

## See also

[get_MY_inits](https://dd-harp.github.io/ramp.xds/reference/get_MY_inits.md),
[get_MY_vars](https://dd-harp.github.io/ramp.xds/reference/get_MY_vars.md),
[get_MY_orbits](https://dd-harp.github.io/ramp.xds/reference/get_MY_orbits.md)
