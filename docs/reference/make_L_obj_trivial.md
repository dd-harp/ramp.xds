# Make `L_obj` for `trivial` (**L** component)

The number of emerging adults is a function \$\$\Lambda S(t) T(t)
K(t)\$\$ where

- \\\Lambda\\ or `Lambda` is the mean number of adult female mosquitoes
  emerging per day

- \\S(t)\\ or `F_season` is a seasonal pattern function (ideally, with
  an average annual mean of 1)

- \\T(t)\\ or `F_trend` is a trend pattern function (ideally, with an
  average value of 1)

- \\K(t)\\ or `F_shock` is a perturbation function (by default, it is
  set to `F_one`)

## Usage

``` r
make_L_obj_trivial(
  nHabitats,
  options = list(),
  Lambda = 1000,
  F_season = F_one,
  F_trend = F_one,
  F_shock = F_one
)
```

## Arguments

- nHabitats:

  the number of habitats in the model

- options:

  a [list](https://rdrr.io/r/base/list.html) that overwrites default
  values

- Lambda:

  vector of mean emergence rates from each aquatic habitat

- F_season:

  the seasonal pattern function

- F_trend:

  the trend function

- F_shock:

  the shock function

## Value

a [list](https://rdrr.io/r/base/list.html): an L module object
