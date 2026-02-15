# Compute the Steady State of `dLdt.basicL` (**L** Component)

Given an egg deposition rate `eta,` return a steady state value for the
equations in
[dLdt.basicL](https://dd-harp.github.io/ramp.xds/reference/dLdt.basicL.md)

## Usage

``` r
# S3 method for class 'basicL_ode'
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

the values of \\L\\ at the steady state

## Note

This function does not use deSolve
