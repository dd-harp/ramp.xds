# Parse **L** Component Outputs

After solving a dynamical system, parse the outputs and return the
variables by name in a list.

## Usage

``` r
parse_Lorbits(outputs, pars, s)
```

## Arguments

- outputs:

  a [matrix](https://rdrr.io/r/base/matrix.html) with the solutions to a
  dynamical system

- pars:

  an **`xds`** object

- s:

  the species index

## Value

a [list](https://rdrr.io/r/base/list.html)

## Note

This method dispatches on the class of `pars$Lpar[[s]]`
