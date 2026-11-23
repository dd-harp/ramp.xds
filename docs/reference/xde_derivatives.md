# Compute Derivatives

The function to compute the derivatives dispatches on
`class(xds_obj$frame)`.

## Usage

``` r
xde_derivatives(t, y, xds_obj)
```

## Arguments

- t:

  time

- y:

  dependent variables

- xds_obj:

  an **`xds`** model object

## Value

a [list](https://rdrr.io/r/base/list.html) containing the vector of all
state derivatives

## See also

[xds_solve](https://dd-harp.github.io/ramp.xds/reference/xds_solve.md)
