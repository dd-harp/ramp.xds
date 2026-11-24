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

a [numeric](https://rdrr.io/r/base/numeric.html) vector of derivatives
in the module's state-variable order

## Note

Dispatch on the module object at `xds_obj$MY_obj[[s]]`.
