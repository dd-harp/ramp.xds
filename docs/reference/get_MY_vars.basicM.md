# Get the state variables for `basicM` (**MY**)

Extract the `basicM` variables from the state variables vector \\(y\\),
parse, and return the variables by name in a list.

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

a [list](https://rdrr.io/r/base/list.html)

## State Variables

- `M`:

  density of adult mosquitoes

- `P`:

  density of parous adult mosquitoes: those that have blood-fed at least
  once
