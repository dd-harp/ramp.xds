# Parse **XH** Outputs

After solving, this function extracts the values of the dependent
variables for the **XH** module from the matrix of solutions, often
called the "orbits." The orbits are parsed and attached by name to the
**`xds`** model object.

For differential equations the solution matrix is returned by
[deSolve::deSolve](https://rdrr.io/pkg/deSolve/man/deSolve.html).
Discrete time systems return a matrix that has the same shape.

## Usage

``` r
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
