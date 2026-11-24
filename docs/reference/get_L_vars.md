# Get the state variables (**L**)

Extract the **L** variables from the state variables vector \\(y\\),
parse, and return the variables by name in a list.

## Usage

``` r
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

a named [list](https://rdrr.io/r/base/list.html): the variables of
\\(\cal L\\) by name
