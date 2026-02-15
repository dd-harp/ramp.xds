# Get the *Pf*PR from a Malaria Model

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

  an **`xds`** object

- method:

  the method used for computing *Pf*PR

- i:

  the host species index

## Value

none

## Note

The method assigns `class(method)='method'` then dispatches on
`'method'`
