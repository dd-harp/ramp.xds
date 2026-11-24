# Get the state variables for `basicL` (**L**)

Extract the state variables for the `basicL` module from `y` and return
them by name in a list for use during model solving.

## Usage

``` r
# S3 method for class 'basicL'
get_L_vars(y, xds_obj, s)
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

Variable indices are stored at `xds_obj$L_obj[[s]]$ix`.
