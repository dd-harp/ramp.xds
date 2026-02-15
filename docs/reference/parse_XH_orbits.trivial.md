# parse the output of deSolve and return variables for the trivial model

Implements
[parse_XH_orbits](https://dd-harp.github.io/ramp.xds/reference/parse_XH_orbits.md)
for the trivial model

## Usage

``` r
# S3 method for class 'trivial'
parse_XH_orbits(outputs, xds_obj, i)
```

## Arguments

- outputs:

  an output matrix returned by
  [deSolve::deSolve](https://rdrr.io/pkg/deSolve/man/deSolve.html)

- xds_obj:

  an **`xds`** model object

- i:

  the host species index

## Value

none
