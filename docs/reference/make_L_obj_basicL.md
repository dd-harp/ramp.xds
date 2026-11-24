# Make a module object for `basicL` (**L**)

Create a `basicL` module object and initialize its parameters. Inspect
parameters with
[get_L_pars.basicL](https://dd-harp.github.io/ramp.xds/reference/get_L_pars.basicL.md)
and change supported values with
[change_L_pars.basicL](https://dd-harp.github.io/ramp.xds/reference/change_L_pars.basicL.md).
Values in `options` override parameter arguments.

## Usage

``` r
make_L_obj_basicL(
  nHabitats,
  options = list(),
  psi = 1/8,
  xi = 0,
  phi = 1/8,
  theta = 1/100
)
```

## Arguments

- nHabitats:

  the number of habitats in the model

- options:

  a named [list](https://rdrr.io/r/base/list.html)

- psi:

  maturation rates for each aquatic habitat

- xi:

  delayed maturation in response to mean crowding

- phi:

  density-independent mortality rates for each aquatic habitat

- theta:

  slope of density-dependent mortality for each aquatic habitat

## Value

a [list](https://rdrr.io/r/base/list.html): an **L** module object

## Parameters

- `psi` or \\(\psi\\):

  maturation rate

- `xi` or \\(\xi\\):

  delayed maturation parameter in response to mean crowding

- `phi` or \\(\phi\\):

  density-independent mortality rate

- `theta` or \\(\theta\\):

  slope of density-dependent mortality

## See also

[get_L_pars.basicL](https://dd-harp.github.io/ramp.xds/reference/get_L_pars.basicL.md),
[change_L_pars.basicL](https://dd-harp.github.io/ramp.xds/reference/change_L_pars.basicL.md)

Called by:
[setup_L_obj.basicL](https://dd-harp.github.io/ramp.xds/reference/setup_L_obj.basicL.md).
Related:
[dLdt.basicL](https://dd-harp.github.io/ramp.xds/reference/dLdt.basicL.md)
&
[Update_Lt.basicL](https://dd-harp.github.io/ramp.xds/reference/Update_Lt.basicL.md)
