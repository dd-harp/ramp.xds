# List **L** Component Variables

Extract the variables describing **L** Component states for the
\\s^{th}\\ species and return them as a named list

## Usage

``` r
list_Lvars(y, pars, s)
```

## Arguments

- y:

  the variables

- pars:

  an **`xds`** object

- s:

  the vector species index

## Value

a named [list](https://rdrr.io/r/base/list.html): the variables of
\\\cal L\\ by name

## Note

This method dispatches on the class of `pars$Lpar[[s]]`.
