# Get the human blood fraction (**MY**)

Return the stored realized human blood fraction per patch.

## Usage

``` r
get_q(xds_obj, s = 1)
```

## Arguments

- xds_obj:

  an **`xds`** model object

- s:

  the vector species index

## Value

a [numeric](https://rdrr.io/r/base/numeric.html) vector of length
`nPatches`

## Details

Read the realized parameter without evaluating its port or applying
effect sizes. Run bionomics and effect-size application to refresh
port-backed values.

## Note

Dispatch on the module object at `xds_obj$MY_obj[[s]]`.
