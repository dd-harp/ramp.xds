# Get the *Pf*PR from a Malaria Model

This method assigns `class(method)=method` and then dispatches on
"type." Options are:

- `true` for the true *Pf*PR

- `lm` for the *Pf*PR by light microscopy

- `rdt` for the *Pf*PR by RDT

- `pcr` for the *Pf*PR by PCR

## Usage

``` r
get_PR(xds_obj, i = 1, method = "true")
```

## Arguments

- xds_obj:

  an **`xds`** object

- i:

  the host species index

- method:

  the method used for computing *Pf*PR

## Value

none
