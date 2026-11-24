# Make a module object for `GeM` (**MY**)

Create a `GeM` module object and initialize its parameters. Inspect
parameters with
[get_MY_pars.GeM](https://dd-harp.github.io/ramp.xds/reference/get_MY_pars.GeM.md)
and change supported values with
[change_MY_pars.GeM](https://dd-harp.github.io/ramp.xds/reference/change_MY_pars.GeM.md).
Values in `options` override parameter arguments.

## Usage

``` r
make_MY_obj_GeM(
  nPatches,
  options = list(),
  eip = 12,
  g = 1/12,
  sigma = 1/8,
  mu = 0,
  f = 0.3,
  q = 0.95,
  nu = 1,
  eggs_per_batch = 60
)
```

## Arguments

- nPatches:

  is the number of patches, an integer

- options:

  a [list](https://rdrr.io/r/base/list.html) of values that overrides
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

  the oviposition rate: the number of egg batches laid in each patch,
  per mosquito, per day

- eggs_per_batch:

  number of eggs per batch

## Value

a [list](https://rdrr.io/r/base/list.html)

## See also

[get_MY_pars.GeM](https://dd-harp.github.io/ramp.xds/reference/get_MY_pars.GeM.md),
[change_MY_pars.GeM](https://dd-harp.github.io/ramp.xds/reference/change_MY_pars.GeM.md)

[GeM](https://dd-harp.github.io/ramp.xds/reference/GeM.md),
[setup_MY_obj.GeM](https://dd-harp.github.io/ramp.xds/reference/setup_MY_obj.GeM.md)
