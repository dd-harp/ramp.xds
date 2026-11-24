# Compute adult mosquito emergence (**L**)

Return adult female mosquito emergence per habitat. This is a rate for
differential equations or a number for discrete-time systems.

## Usage

``` r
F_emerge(t, y, xds_obj, s)
```

## Arguments

- t:

  time

- y:

  dependent variables

- xds_obj:

  an **`xds`** model object

- s:

  the species index

## Value

a [numeric](https://rdrr.io/r/base/numeric.html) vector of length
`nHabitats`

## Note

Dispatch on the module object at `xds_obj$L_obj[[s]]`.

## See also

[Emergence](https://dd-harp.github.io/ramp.xds/reference/Emergence.md)
