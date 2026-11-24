# Parse outputs for `hMoI` (**XH**)

Extract the model state outputs for the `hMoI` module from `outputs` and
return them by name in a list. Return `m1`, `m2`, and a matrix `H` with
one row per output time and `nStrata[i]` columns. The fixed population
parameter `H` is repeated across output times.

## Usage

``` r
# S3 method for class 'hMoI'
parse_XH_orbits(outputs, xds_obj, i)
```

## Arguments

- outputs:

  a [matrix](https://rdrr.io/r/base/matrix.html) of model state outputs

- xds_obj:

  an **`xds`** model object

- i:

  the host species index

## Value

a [list](https://rdrr.io/r/base/list.html)

## Note

Variable indices are stored at `xds_obj$XH_obj[[i]]$ix`.

## See also

[hMoI](https://dd-harp.github.io/ramp.xds/reference/hMoI.md)
