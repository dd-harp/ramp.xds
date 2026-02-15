# Compute Emergent Adults for `trivial` (**L** Component)

The number of emerging adults is a function \$\$\Lambda S(t) T(t)\$\$
where

- \\\Lambda\\ or `Lambda` is the mean number of adult female mosquitoes
  emerging per day

- \\S(t)\\ or `F_season` is a seasonal signal (ideally, with an average
  annual mean of 1)

- \\T(t)\\ or `F_trend` is a function returning a trend (ideally, with
  an average value of 1)

- \\P(t)\\ or `F_shock` is a function describing a perturbation (by
  default, set to 1)

## Usage

``` r
# S3 method for class 'trivial'
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
