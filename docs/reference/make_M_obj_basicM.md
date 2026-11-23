# Make the module object for `basicM` (**MY**)

Make the module object for `basicM` (**MY**)

## Usage

``` r
make_M_obj_basicM(
  nPatches,
  options = list(),
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

  a named list: named values override defaults

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

  the oviposition rate: the number of egg batches laid in each patch,
  per mosquito, per day

- eggsPerBatch:

  number of eggs per batch

## Value

a [list](https://rdrr.io/r/base/list.html)

## See also

[basicM](https://dd-harp.github.io/ramp.xds/reference/basicM.md),
[setup_MY_obj.basicM](https://dd-harp.github.io/ramp.xds/reference/setup_MY_obj.basicM.md)
