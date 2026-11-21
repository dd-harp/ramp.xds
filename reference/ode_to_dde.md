# Compute as DDE

Set up a differential equation model to be solved using
[`deSolve::dede`](https://rdrr.io/pkg/deSolve/man/dede.html).

## Usage

``` r
ode_to_dde(xds_obj)
```

## Arguments

- xds_obj:

  an **`xds`** model object

## Value

an **`xds`** model object

## Details

For differential equations, the value of `xds_obj$xde` is set to `ode`
by default. This utility is called during setup to change `xds_obj$xde`
from `ode` to `dde` when a module requires a delay.
