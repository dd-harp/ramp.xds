# Parse outputs (**MY**)

Extract the model state outputs for the selected **MY** module and
return them by name in a list.

## Usage

``` r
parse_MY_orbits(outputs, xds_obj, s)
```

## Arguments

- outputs:

  a [matrix](https://rdrr.io/r/base/matrix.html) of model state outputs

- xds_obj:

  an **`xds`** model object

- s:

  the vector species index

## Value

a [list](https://rdrr.io/r/base/list.html)

## Note

Dispatch on the module object at `xds_obj$MY_obj[[s]]`. These methods
process output trajectories after solving, rather than a single state
vector during solving. They return results without modifying the model
object; the output-processing pipeline stores the parsed results at
`xds_obj$outputs$orbits$MY[[s]]`.
