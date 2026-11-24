# Parse outputs (**XH**)

Extract the model state outputs for the selected **XH** module and
return them by name in a list.

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

## Note

Dispatch on the module object at `xds_obj$XH_obj[[i]]`. These methods
process output trajectories after solving, rather than a single state
vector during solving. They return results without modifying the model
object; the output-processing pipeline stores the parsed results at
`xds_obj$outputs$orbits$XH[[i]]`.
