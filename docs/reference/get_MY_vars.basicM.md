# Get the state variables for `basicM` (**MY**)

Extract the state variables for the `basicM` module from `y` and return
them by name in a list for use during model solving.

## Usage

``` r
# S3 method for class 'basicM'
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

## State Variables

- `M`:

  density of adult mosquitoes

- `P`:

  density of parous adult mosquitoes: those that have blood-fed at least
  once
