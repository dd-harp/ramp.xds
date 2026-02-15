# This function computes the negative derivative of the EIP as a function of time

Implements
[d_F_eip_dt](https://dd-harp.github.io/ramp.xds/reference/d_F_eip_dt.md)
for the static_xde model (deip_dt=0)

## Usage

``` r
# S3 method for static
d_F_eip_dt(t, vars, eip_par)
```

## Arguments

- t:

  current simulation time

- vars:

  exogenous variables

- eip_par:

  a [list](https://rdrr.io/r/base/list.html)

## Value

[numeric](https://rdrr.io/r/base/numeric.html)
