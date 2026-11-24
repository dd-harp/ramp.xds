# Get the state variables for `RMdts` (**MY**)

Extract the state variables for the `RMdts` module from `y` and return
them by name in a list for use during model solving. Return `Y` as an
`nPatches` by `max_eip` cohort matrix.

## Usage

``` r
# S3 method for class 'RMdts'
get_MY_vars(y, xds_obj, s)
```

## Arguments

- y:

  dependent variables

- xds_obj:

  an **`xds`** model object

- s:

  the vector species index

## Value

a named [list](https://rdrr.io/r/base/list.html) of state variables and
module-specific derived quantities

## Note

Variable indices are stored at `xds_obj$MY_obj[[s]]$ix`.
