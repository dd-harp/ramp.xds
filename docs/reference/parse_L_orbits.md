# Parse outputs (**L**)

Extract the model state outputs for the selected **L** module and return
them by name in a list.

## Usage

``` r
parse_L_orbits(outputs, xds_obj, s)
```

## Arguments

- outputs:

  a [matrix](https://rdrr.io/r/base/matrix.html) of model state outputs

- xds_obj:

  an **`xds`** model object

- s:

  the species index

## Value

a [list](https://rdrr.io/r/base/list.html)

## Note

Dispatch on the module object at `xds_obj$L_obj[[s]]`. These methods
process output trajectories after solving, rather than a single state
vector during solving. They return results without modifying the model
object; the output-processing pipeline stores the parsed results at
`xds_obj$outputs$orbits$L[[s]]`.
