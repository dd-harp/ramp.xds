# Compute the steady state of `dLdt.basicL` (**L** component)

Given an egg deposition rate `eta`, return a steady state value for the
equations in
[dLdt.basicL](https://dd-harp.github.io/ramp.xds/reference/dLdt.basicL.md)

## Usage

``` r
# S3 method for class 'xde'
steady_state_L_basicL(eta, xds_obj, s = 1)
```

## Arguments

- eta:

  the number of eggs laid in each habitat, per day

- xds_obj:

  an **`xds`** model object

- s:

  the species index

## Value

a named [list](https://rdrr.io/r/base/list.html) containing the values
of \\(L\\) at the steady state

## Note

This function does not use deSolve

## ODE

The derivatives in
[dLdt.basicL](https://dd-harp.github.io/ramp.xds/reference/dLdt.basicL.md)
are: \$\$dL/dt = \eta - (\psi e^{-\xi L} + \phi + \theta L) L\$\$
