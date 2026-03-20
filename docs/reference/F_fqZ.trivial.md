# Net Infectious Biting Rate

Returns \$\$F\_{fqZ}(t) = fqZ S(t) T(t) K(t)\$\$ where

- \\f\\ is the feeding rate

- \\q\\ the human fraction

- \\Z\\ is the density of infectious mosquitoes, per patch

- \\S(t)\\ or `F_season` is a seasonal pattern

- \\T(t)\\ or `F_trend` is a trend pattern

- \\K(t)\\ or `F_shock` is a perturbation

## Usage

``` r
# S3 method for class 'trivial'
F_fqZ(t, y, xds_obj, s)
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
