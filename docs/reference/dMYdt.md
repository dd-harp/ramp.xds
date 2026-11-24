# Compute the derivatives (**MY**)

Compute the derivatives of the adult mosquito state variables using the
selected **MY** module. See the module-specific documentation for
equations and references.

## Usage

``` r
dMYdt(t, y, xds_obj, s)
```

## Arguments

- t:

  time

- y:

  dependent variables

- xds_obj:

  an **`xds`** model object

- s:

  the vector species index

## Value

Derivatives for an adult mosquito model, a
[vector](https://rdrr.io/r/base/vector.html)
