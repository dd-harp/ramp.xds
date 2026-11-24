# Make a module object for `hMoI` (**XH**)

Create an `hMoI` module object and initialize its parameters. Inspect
parameters with
[get_XH_pars.hMoI](https://dd-harp.github.io/ramp.xds/reference/get_XH_pars.hMoI.md)
and change supported values with
[change_XH_pars.hMoI](https://dd-harp.github.io/ramp.xds/reference/change_XH_pars.hMoI.md).
Values in `options` override parameter arguments.

MoI stands for Multiplicity of Infection, and refers to malarial
superinfection.

## Usage

``` r
make_XH_obj_hMoI(
  nStrata,
  options = list(),
  b = 0.55,
  r1 = 1/180,
  r2 = 1/70,
  c1 = 0.015,
  c2 = 0.15,
  H = 1
)
```

## Arguments

- nStrata:

  is the number of human population strata

- options:

  a [list](https://rdrr.io/r/base/list.html) that overrides default
  values

- b:

  transmission probability (efficiency) from mosquito to human

- r1:

  recovery rate from inapparent infections

- r2:

  recovery rate from patent infections

- c1:

  transmission probability (efficiency) from inapparent human infections
  to mosquito

- c2:

  transmission probability (efficiency) from patent human infections to
  mosquito

- H:

  human population density

## Value

a [list](https://rdrr.io/r/base/list.html)

## See also

[get_XH_pars.hMoI](https://dd-harp.github.io/ramp.xds/reference/get_XH_pars.hMoI.md),
[change_XH_pars.hMoI](https://dd-harp.github.io/ramp.xds/reference/change_XH_pars.hMoI.md)

[hMoI](https://dd-harp.github.io/ramp.xds/reference/hMoI.md)
