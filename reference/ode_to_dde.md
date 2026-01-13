# Compute as DDE

Setup a differential equation model to be solved using `deSolve:dede`

## Usage

``` r
ode_to_dde(xds_obj)
```

## Arguments

- xds_obj:

  an **`xds`** model object

## Value

a **`ramp.xds`** model object

## Details

For differential equations, the value of `xds_obj$xde` is set to `ode`
by default. This utility gets called by delay differential equation
modules to change `xds_obj$xde` from `ode` to `dde`
