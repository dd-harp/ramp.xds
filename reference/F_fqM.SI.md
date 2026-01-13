# Compute Net Blood Feeding by Mosquitoes for `SI`

The variable \\M\\ is the density of mosquitoes. The model blood feeding
**parameters** are:

- \\f\\ is the overall blood feeding rate

- \\q\\ is the human fraction for blood feeding The daily HBR for the
  human / host population strata is \\\beta \cdot fqM\\

## Usage

``` r
# S3 method for class 'SI'
F_fqM(t, y, xds_obj, s)
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

a [numeric](https://rdrr.io/r/base/numeric.html) vector of length
`nPatches`
