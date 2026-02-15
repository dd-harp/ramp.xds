# parse the outputs and return the variables by name in a list

This method dispatches on the type of `xds_obj$MY_obj`. It computes the
variables by name and returns a named list.

## Usage

``` r
parse_MY_orbits(outputs, xds_obj, s)
```

## Arguments

- outputs:

  a [matrix](https://rdrr.io/r/base/matrix.html) of outputs from deSolve

- xds_obj:

  an **`xds`** model object

- s:

  the species index

## Value

[list](https://rdrr.io/r/base/list.html)
