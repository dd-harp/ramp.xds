# Make inits for GeRM adult mosquito model

Make inits for GeRM adult mosquito model

## Usage

``` r
make_MY_inits_GeRM(
  nPatches,
  Upsilon,
  options = list(),
  M = 5,
  P = 1,
  Y = 1,
  Z = 1
)
```

## Arguments

- nPatches:

  the number of patches in the model

- Upsilon:

  a matrix describing survival and dispersal through the EIP

- options:

  a [list](https://rdrr.io/r/base/list.html) of values that overwrites
  the defaults

- M:

  total mosquito density at each patch

- P:

  total parous mosquito density at each patch

- Y:

  infected mosquito density at each patch

- Z:

  infectious mosquito density at each patch

## Value

a [list](https://rdrr.io/r/base/list.html)
