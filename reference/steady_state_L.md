# Compute steady states for **L** Component Modules

For differential equation models, compute steady states as a function of
daily eggs laid, \\\eta\\

## Usage

``` r
steady_state_L(eta, xds_obj, s = 1)
```

## Arguments

- eta:

  the egg-laying rate

- xds_obj:

  an **`xds`** model object

- s:

  the species index

## Value

a named [list](https://rdrr.io/r/base/list.html): values of the state
variables at the steady state

## Note

This method dispatches on the class of `L_obj`.
