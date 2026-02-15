# Return the parameters as a list

Parameter values for the \\i^{th}\\ host are stored as
`xds_obj$XH_obj[[i]]`. This returns the stored parameter values as a
list.

## Usage

``` r
# S3 method for class 'hMoI'
get_XH_pars(xds_obj, i = 1)
```

## Arguments

- xds_obj:

  an **`xds`** model object

- i:

  the host species index

## Value

a [list](https://rdrr.io/r/base/list.html)

## See also

[make_XH_obj_hMoI](https://dd-harp.github.io/ramp.xds/reference/make_XH_obj_hMoI.md)
