# parse the output of deSolve and return variables for the RMdts model

Implements
[parse_MY_orbits](https://dd-harp.github.io/ramp.xds/reference/parse_MY_orbits.md)
for the RMdts model

## Usage

``` r
# S3 method for class 'RMdts'
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

a [list](https://rdrr.io/r/base/list.html)
