# Get the *Pf*PR from a malaria model

`'method'` should be

- `true` for the true *Pf*PR (default)

- `lm` for the *Pf*PR by light microscopy

- `rdt` for the *Pf*PR by RDT

- `pcr` for the *Pf*PR by PCR

## Usage

``` r
get_PR(xds_obj, method = "true", i = 1)
```

## Arguments

- xds_obj:

  an **`xds`** model object

- method:

  the method used for computing *Pf*PR

- i:

  the host species index

## Value

a [list](https://rdrr.io/r/base/list.html)

## Note

The method assigns `class(method)='method'` then dispatches on
`'method'`
