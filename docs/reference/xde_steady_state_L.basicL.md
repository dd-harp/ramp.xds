# Compute the Steady State of `dLdt.basicL` (**L** Component)

Given an egg deposition rate `eta,` return a steady state value for the
equations in
[dLdt.basicL](https://dd-harp.github.io/ramp.xds/reference/dLdt.basicL.md)

## Usage

``` r
# S3 method for basicL
xde_steady_state_L(eta, Lpar)
```

## Arguments

- eta:

  the egg-laying rate

- Lpar:

  a list that defines an xde model

## Value

the values of \\L\\ at the steady state

## Note

This function does not use deSolve
