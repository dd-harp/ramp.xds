# Make a module object for `SIS` (**XH**)

Create an `SIS` module object and initialize its parameters. Inspect
parameters with
[get_XH_pars.SIS](https://dd-harp.github.io/ramp.xds/reference/get_XH_pars.SIS.md)
and change supported values with
[change_XH_pars.SIS](https://dd-harp.github.io/ramp.xds/reference/change_XH_pars.SIS.md).
Values in `options` override parameter arguments.

## Usage

``` r
make_XH_obj_SIS(
  nStrata,
  options = list(),
  b = 0.55,
  r = 1/180,
  c = 0.15,
  rho = 0,
  sigma = 0,
  q = 0.8
)
```

## Arguments

- nStrata:

  is the number of population strata

- options:

  a named list with parameter values to override defaults

- b:

  transmission probability (efficiency) from mosquito to human

- r:

  recovery rate

- c:

  transmission probability (efficiency) from human to mosquito

- rho:

  the fraction of incident infections that are treated and cured

- sigma:

  the rate prevalent infections are treated and cured

- q:

  test positivity

## Value

an **XH** model object

## Parameters

The parameters that are returned by
[get_XH_pars.SIS](https://dd-harp.github.io/ramp.xds/reference/get_XH_pars.SIS.md)
and also modifiable by
[change_XH_pars.SIS](https://dd-harp.github.io/ramp.xds/reference/change_XH_pars.SIS.md)
describe the dynamics or test positivity.

- `b`:

  the fraction of infective bites that would cause an infection

- `r`:

  clearance rate for infections

- `c`:

  the probability an infected person would infect a mosquito

- `rho` or \\\rho\\:

  the fraction of incident infections that are treated and cured

- `sigma` or \\\sigma\\:

  the rate that prevalent infections are treated and cured

- `q`:

  test positivity

## See also

[get_XH_pars.SIS](https://dd-harp.github.io/ramp.xds/reference/get_XH_pars.SIS.md),
[change_XH_pars.SIS](https://dd-harp.github.io/ramp.xds/reference/change_XH_pars.SIS.md)

[SIS](https://dd-harp.github.io/ramp.xds/reference/SIS.md)
