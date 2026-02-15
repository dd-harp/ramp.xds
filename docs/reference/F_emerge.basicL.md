# Compute Emergent Adults for `basicL` (**L** Component)

The number of adults emerging from the habitats, per day, is: \$\$\psi
e^{-\xi L} L.\$\$

## Usage

``` r
# S3 method for class 'basicL'
F_emerge(t, y, xds_obj, s)
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
`nHabitats`

## See also

[dLdt.basicL](https://dd-harp.github.io/ramp.xds/reference/dLdt.basicL.md)
