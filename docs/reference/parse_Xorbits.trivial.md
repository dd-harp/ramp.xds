# Parse the output of deSolve and return variables for the trivial model

Implements
[parse_Xorbits](https://dd-harp.github.io/ramp.xds/reference/parse_Xorbits.md)
for the trivial model

## Usage

``` r
# S3 method for trivial
parse_Xorbits(outputs, pars, i)
```

## Arguments

- outputs:

  a [matrix](https://rdrr.io/r/base/matrix.html) of outputs from deSolve

- pars:

  an **`xds`** object

- i:

  the host species index

## Value

none
