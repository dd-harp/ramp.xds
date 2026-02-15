# Eggs Laying in Habitats

Computes egg distribution for the aquatic habitats

## Usage

``` r
F_eta(eggs_laid, O_matrix, Q, Qall)
```

## Arguments

- eggs_laid:

  the number of eggs laid in each patch, a vector of length `nPatches`

- O_matrix:

  the egg laying matrix

- Q:

  larval habitat availability

- Qall:

  total availability

## Value

a [vector](https://rdrr.io/r/base/vector.html), \\\eta\\ where
\\\left\|\eta\right\|=\\`nHabitats`

## See also

[compute_O_matrix](https://dd-harp.github.io/ramp.xds/reference/compute_O_matrix.md)
