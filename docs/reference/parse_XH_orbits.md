# Parse outputs (**XH**)

After solving, this function extracts the values of the dependent
variables for the **XH** module from the matrix of solutions, often
called the "orbits." The orbits are parsed and attached by name to the
**`xds`** model object.

For differential equations the solution matrix is returned by
[deSolve](https://rdrr.io/pkg/deSolve/man/deSolve.html). Discrete time
systems return a matrix that has the same shape.

## Usage

``` r
parse_XH_orbits(outputs, xds_obj, i)
```

## Arguments

- outputs:

  a [matrix](https://rdrr.io/r/base/matrix.html) of model state outputs

- xds_obj:

  an **`xds`** model object

- i:

  the host species index

## Value

a [list](https://rdrr.io/r/base/list.html)
