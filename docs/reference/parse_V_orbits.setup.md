# Parse **V** Outputs

Return an empty list for the auxiliary-variable placeholder.

## Usage

``` r
# S3 method for class 'setup'
parse_V_orbits(outputs, xds_obj, i)
```

## Arguments

- outputs:

  an output matrix returned by
  [deSolve](https://rdrr.io/pkg/deSolve/man/deSolve.html)

- xds_obj:

  an **`xds`** model object

- i:

  the auxiliary-variable module index

## Value

an empty [list](https://rdrr.io/r/base/list.html)
