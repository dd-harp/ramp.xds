# parse **L** Component Outputs

After solving a dynamical system, parse the outputs and return the
variables by name in a list.

## Usage

``` r
parse_L_orbits(outputs, xds_obj, s)
```

## Arguments

- outputs:

  a [matrix](https://rdrr.io/r/base/matrix.html) with the solutions to a
  dynamical system

- xds_obj:

  an **`xds`** model object

- s:

  the species index

## Value

a [list](https://rdrr.io/r/base/list.html)

## Note

This method dispatches on the class of `xds_obj$L_obj[[s]]`
