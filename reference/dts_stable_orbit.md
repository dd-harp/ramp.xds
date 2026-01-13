# Solve for the steady state or stable orbit of a system of equations

This method dispatches on the type of `xds_obj$dts`.

## Usage

``` r
dts_stable_orbit(xds_obj, Tburn = 10, yr = 365)
```

## Arguments

- xds_obj:

  an **`xds`** model object

- Tburn:

  the number of steps to burn

- yr:

  the number of time steps in a year

## Value

a [list](https://rdrr.io/r/base/list.html)
