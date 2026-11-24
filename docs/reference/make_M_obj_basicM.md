# Make a module object for `basicM` (**MY**)

Create a `basicM` module object and initialize its parameters. Inspect
parameters with
[get_MY_pars.basicM](https://dd-harp.github.io/ramp.xds/reference/get_MY_pars.basicM.md)
and change supported values with
[change_MY_pars.basicM](https://dd-harp.github.io/ramp.xds/reference/change_MY_pars.basicM.md).
Values in `options` override parameter arguments.

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

[get_MY_pars.basicM](https://dd-harp.github.io/ramp.xds/reference/get_MY_pars.basicM.md),
[change_MY_pars.basicM](https://dd-harp.github.io/ramp.xds/reference/change_MY_pars.basicM.md)

[basicM](https://dd-harp.github.io/ramp.xds/reference/basicM.md),
[setup_MY_obj.basicM](https://dd-harp.github.io/ramp.xds/reference/setup_MY_obj.basicM.md)
