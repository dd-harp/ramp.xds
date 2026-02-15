# Make parameters for GeRM ODE adult mosquito model

Make parameters for GeRM ODE adult mosquito model

## Usage

``` r
make_MY_obj_GeRM(
  nPatches,
  options = list(),
  eip = 12,
  g = 1/12,
  sigma = 1/8,
  mu = 0,
  f = 0.3,
  q = 0.95,
  nu = 1,
  eggsPerBatch = 60
)
```

## Arguments

- nPatches:

  is the number of patches, an integer

- options:

  a [list](https://rdrr.io/r/base/list.html) of values that overwrites
  the defaults

- eip:

  the extrinsic incubation period

- g:

  mosquito mortality rate

- sigma:

  emigration rate

- mu:

  emigration loss

- f:

  feeding rate

- q:

  human blood fraction

- nu:

  oviposition rate, per mosquito

- eggsPerBatch:

  eggs laid per oviposition

## Value

a [list](https://rdrr.io/r/base/list.html)
