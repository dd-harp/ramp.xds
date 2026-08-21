# Dynamically update a K Matrix

A port function to updates the mosquito dispersal matrix dynamically.

## Usage

``` r
F_K_matrix(t, xds_obj, s)
```

## Arguments

- t:

  current simulation time

- xds_obj:

  an **`xds`** model object

- s:

  vector species index

## Value

a [numeric](https://rdrr.io/r/base/numeric.html) vector of length
`nPatches`

## Note

The port object is called `K_obj`
