# Compute steady states for `basicL` (**L**)

Given the number of eggs laid in each habitat, per day, `eta`, return
steady-state values for the discrete-time system in
[Update_Lt.basicL](https://dd-harp.github.io/ramp.xds/reference/Update_Lt.basicL.md).
Minimize the sum of squared fixed-point residuals using
[stats::nlm](https://rdrr.io/r/stats/nlm.html).

## Usage

``` r
# S3 method for class 'dts'
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

## DTS

The one-day update in
[Update_Lt.basicL](https://dd-harp.github.io/ramp.xds/reference/Update_Lt.basicL.md)
is: \$\$L_t = \eta + e^{-(\psi e^{-\xi L\_{t-1}} + \phi + \theta
L\_{t-1})} L\_{t-1}\$\$
