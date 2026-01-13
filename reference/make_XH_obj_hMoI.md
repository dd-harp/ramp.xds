# Make parameters for hybrid MoI human model

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
  c2 = 0.15
)
```

## Arguments

- nStrata:

  is the number of human population strata

- options:

  a [list](https://rdrr.io/r/base/list.html) that overwrites default
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

## Value

none
