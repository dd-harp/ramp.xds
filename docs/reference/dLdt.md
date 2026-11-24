# Compute the derivatives (**L**)

Compute the derivatives of the aquatic mosquito state variables using
the selected **L** module. See the module-specific documentation for
equations and references.

## Usage

``` r
dLdt(t, y, xds_obj, s)
```

## Arguments

- t:

  time

- y:

  dependent variables

- xds_obj:

  an **`xds`** model object

- s:

  the species index

## Value

a [numeric](https://rdrr.io/r/base/numeric.html) vector of derivatives
in the module's state-variable order; empty for modules without state
variables

## Note

Dispatch on the module object at `xds_obj$L_obj[[s]]`.
