# Make parameters for trivial human model

Make parameters for trivial human model

## Usage

``` r
make_XH_obj_trivial(
  nPatches,
  options,
  kappa = 0.1,
  HPop = 1,
  F_season = F_one,
  F_trend = F_one,
  F_shock = F_one
)
```

## Arguments

- nPatches:

  the number of patches

- options:

  a [list](https://rdrr.io/r/base/list.html)

- kappa:

  net infectiousness

- HPop:

  initial human population density

- F_season:

  the seasonal pattern function

- F_trend:

  the trend function

- F_shock:

  the shock function

## Value

a [list](https://rdrr.io/r/base/list.html)
