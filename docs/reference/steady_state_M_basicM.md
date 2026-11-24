# Compute steady states for `basicM` (**MY**)

Select the `basicM` steady-state calculation using `xds`. Both
differential equations and discrete-time systems are supported.

## Usage

``` r
steady_state_M_basicM(Lambda, xds_obj, s = 1)
```

## Arguments

- Lambda:

  the daily emergence rate of adult mosquitoes

- xds_obj:

  an **`xds`** model object

- s:

  the vector species index

## Value

the steady states as a named
[numeric](https://rdrr.io/r/base/numeric.html) vector
