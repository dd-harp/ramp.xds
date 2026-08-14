# Make parameters for trivial aquatic mosquito model

Make parameters for trivial aquatic mosquito model

## Usage

``` r
make_MY_obj_trivial(
  nPatches,
  options,
  f = 1,
  q = 1,
  Z = 1,
  eggs = 1,
  F_season = F_one,
  F_trend = F_one,
  F_shock = F_one
)
```

## Arguments

- nPatches:

  an integer

- options:

  a [list](https://rdrr.io/r/base/list.html) of values that overwrites
  the defaults

- f:

  the blood feeding rate

- q:

  the human fraction

- Z:

  the density of infectious mosquitoes

- eggs:

  the mean egg laying rate

- F_season:

  the seasonal pattern function

- F_trend:

  the trend function

- F_shock:

  the shock function

## Value

a [list](https://rdrr.io/r/base/list.html)
