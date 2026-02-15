# Compute steady States for **L** Component Modules

For discrete time systems, compute steady states as a function of daily
eggs laid, \\\eta\\

## Usage

``` r
dts_steady_state_L(eta, Lpar)
```

## Arguments

- eta:

  the egg-laying rate

- Lpar:

  a list that defines an xde model

## Value

a named [list](https://rdrr.io/r/base/list.html): values of the state
variables at the steady state

## Note

This method dispatches on the class of `Lpar`.
