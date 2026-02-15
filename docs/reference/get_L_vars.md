# List **L** Component Variables

Extract the variables describing **L** Component states for the
\\s^{th}\\ species and return them as a named list

## Usage

``` r
get_L_vars(y, xds_obj, s)
```

## Arguments

- y:

  the variables

- xds_obj:

  an **`xds`** model object

- s:

  the vector species index

## Value

a named [list](https://rdrr.io/r/base/list.html): the variables of
\\\cal L\\ by name

## Note

This method dispatches on the class of `xds_obj$L_obj[[s]]`.
