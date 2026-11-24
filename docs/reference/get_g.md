# Get the adult mosquito mortality rate (**MY**)

Return the stored realized adult mosquito mortality rate per patch.

## Usage

``` r
get_g(xds_obj, s = 1)
```

## Arguments

- xds_obj:

  an **`xds`** model object

- s:

  the vector species index

## Value

a [numeric](https://rdrr.io/r/base/numeric.html) vector of length
`nPatches`; empty for trivialMY

## Details

Read the realized parameter without evaluating its port or applying
effect sizes. Run bionomics and effect-size application to refresh
port-backed values.

The trivialMY method returns an empty numeric vector.

## Note

Dispatch on the module object at `xds_obj$MY_obj[[s]]`.
