# Net Egg Laying Rate

Returns \$\$F\_{G}(t) = G S(t) T(t) K(t)\$\$ where

- \\G\\ is the number of eggs laid, per patch, per day

- \\S(t)\\ or `F_season` is a seasonal pattern

- \\T(t)\\ or `F_trend` is a trend pattern

- \\K(t)\\ or `F_shock` is a perturbation

## Usage

``` r
# S3 method for class 'trivial'
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

  the vector species index

## Value

a [numeric](https://rdrr.io/r/base/numeric.html) vector of length
`nPatches`
