# Modify parameters due to exogenous forcing by all kinds of control

Implements
[F_eip](https://dd-harp.github.io/ramp.xds/reference/F_eip.md) for the
fixedlag_dts model (the EIP is constant)

## Usage

``` r
# S3 method for fixedlag_dts
F_eip(t, vars, eip_par)
```

## Arguments

- t:

  current simulation time

- vars:

  exogenous variables

- eip_par:

  a [list](https://rdrr.io/r/base/list.html)

## Value

the EIP maturation vector G
[numeric](https://rdrr.io/r/base/numeric.html)
