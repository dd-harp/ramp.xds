# Make inits for RMdts adult mosquito model

Make inits for RMdts adult mosquito model

## Usage

``` r
make_MY_inits_RMdts(
  nPatches,
  max_eip,
  options = list(),
  M = 5,
  P = 1,
  U = 0,
  Y = 1,
  Z = 1
)
```

## Arguments

- nPatches:

  the number of patches in the model

- max_eip:

  the maximum number of EIP cohorts, an
  [integer](https://rdrr.io/r/base/integer.html)

- options:

  a [list](https://rdrr.io/r/base/list.html) of values that overwrites
  the defaults

- M:

  total mosquito density at each patch

- P:

  total parous mosquito density at each patch

- U:

  total uninfected mosquito density at each patch

- Y:

  infected mosquito density at each patch

- Z:

  infectious mosquito density at each patch

## Value

a [list](https://rdrr.io/r/base/list.html)
