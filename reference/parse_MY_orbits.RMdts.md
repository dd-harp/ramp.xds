# Parse outputs for `RMdts` (**MY**)

Extract the model state outputs for the `RMdts` module from `outputs`
and return them by name in a list. Sum the `Y` cohorts within each patch
for each output time, and compute `y = Y/M`, `z = Z/M`, and
`parous = P/M`.

## Usage

``` r
# S3 method for class 'RMdts'
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

Variable indices are stored at `xds_obj$MY_obj[[s]]$ix`. The
output-processing pipeline stores the returned orbits at
`xds_obj$outputs$orbits$MY[[s]]`.
