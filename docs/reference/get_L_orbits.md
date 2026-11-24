# Get orbits (**L**)

Return the saved, parsed orbits (trajectories) for **L** for the
selected species after solving.

## Usage

``` r
get_L_orbits(xds_obj, s = 1)
```

## Arguments

- xds_obj:

  an **`xds`** model object

- s:

  the vector species index

## Value

a [list](https://rdrr.io/r/base/list.html) of module-specific orbits
(trajectories) with a `time` vector

## Note

Orbits are stored at `xds_obj$outputs$orbits$L[[s]]`. The returned list
includes `time` from `xds_obj$outputs$time`. This function does not
parse outputs or modify the model object.

## See also

[parse_L_orbits](https://dd-harp.github.io/ramp.xds/reference/parse_L_orbits.md),
[xds_solve](https://dd-harp.github.io/ramp.xds/reference/xds_solve.md)
