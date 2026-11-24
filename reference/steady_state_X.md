# Compute steady states (**X**)

Compute steady-state infection densities for the selected **X**
component given daily FoI `foi` and fixed host population density `H`.

## Usage

``` r
steady_state_X(foi, H, xds_obj, i = 1)
```

## Arguments

- foi:

  the daily FoI

- H:

  human / host population density

- xds_obj:

  an **`xds`** model object

- i:

  the host species index

## Value

module-specific steady-state values as a named
[list](https://rdrr.io/r/base/list.html) or
[numeric](https://rdrr.io/r/base/numeric.html) vector; see the selected
method for state names

## Note

Dispatch on the module object at `xds_obj$XH_obj[[i]]`. Modules with
equation-system-specific calculations delegate to a second generic that
dispatches on `xds_obj$xds`, selecting `xde` or `dts`. Supported
calculations and assumptions depend on the module. Return values are not
stored on the model object.
