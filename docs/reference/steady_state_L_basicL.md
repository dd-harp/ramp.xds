# Compute steady states for `basicL` (**L**)

Select the `basicL` steady-state calculation using `xds`. Both
differential equations and discrete-time systems are supported.

## Usage

``` r
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
