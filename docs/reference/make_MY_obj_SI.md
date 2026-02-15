# Make parameters for SI ODE adult mosquito model

Make parameters for SI ODE adult mosquito model

## Usage

``` r
make_MY_obj_SI(
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

  a named list: named values overwrite defaults

- eip:

  the extrinSIc incubation period

- g:

  mosquito mortality rate

- sigma:

  emigration rate

- mu:

  fraction lost during emigration

- f:

  feeding rate

- q:

  human blood fraction

- nu:

  ovipoSItion rate, per mosquito

- eggsPerBatch:

  eggs laid per ovipoSItion

## Value

a [list](https://rdrr.io/r/base/list.html)
