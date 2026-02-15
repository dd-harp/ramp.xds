# Compute Derivatives for an Adult Mosquito Model

In the description, each method should include the equations and a
reference to relevant papers in the literature.

## Usage

``` r
dMYdt(t, y, xds_obj, s)
```

## Arguments

- t:

  current simulation time

- y:

  state vector

- xds_obj:

  an **`xds`** model object

- s:

  the species index

## Value

Derivatives for an adult mosquito model, a
[vector](https://rdrr.io/r/base/vector.html)

## Note

This is the `S3` generic. Methods dispatch on `MY_obj[[s]]`
