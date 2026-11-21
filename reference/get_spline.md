# Get spline interpolation points

Get the interpolating points for a spline function.

## Usage

``` r
get_spline(xds_obj, ix = 1)
```

## Arguments

- xds_obj:

  an **`xds`** model object

- ix:

  the vector species index

## Value

a [list](https://rdrr.io/r/base/list.html) containing `tt` and `yy` for
the indexed vector species, or `NULL` when there is no forcing
