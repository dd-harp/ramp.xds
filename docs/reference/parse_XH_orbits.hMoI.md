# Parse outputs for `hMoI` (**XH**)

Implements
[parse_XH_orbits](https://dd-harp.github.io/ramp.xds/reference/parse_XH_orbits.md)
for the `hMoI` model. The fixed population parameter `H` is repeated
across output times.

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

## See also

[hMoI](https://dd-harp.github.io/ramp.xds/reference/hMoI.md)
