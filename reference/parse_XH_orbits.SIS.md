# Parse outputs for `SIS` (**XH**)

Extract the model state outputs for the `SIS` module from `outputs` and
return them by name in a list. Also compute `S = H-I`.

## Usage

``` r
# S3 method for class 'SIS'
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

[SIS](https://dd-harp.github.io/ramp.xds/reference/SIS.md)
