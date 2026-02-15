# Parse **L** Component Variables for `basicL`

The function returns the column representing the variable \\L\\ from a
matrix where each row is a state variable. The variable is returned as a
named list.

## Usage

``` r
# S3 method for basicL
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

a named [list](https://rdrr.io/r/base/list.html)
