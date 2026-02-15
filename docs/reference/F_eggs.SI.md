# Compute Component Egg Laying Rates for `SI`

The density of adult mosquitoes is \\M\\. The **parameters** describing
egg laying by adult mosquitoes are:

- \\\nu\\ or `nu` is the egg laying rate

- \\\xi\\ or `eggsPerBatch` is the number of eggs per batch

The egg laying rate, per patch, is \$\$\nu \xi M\$\$

## Usage

``` r
# S3 method for class 'SI'
F_eggs(t, y, xds_obj, s)
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
