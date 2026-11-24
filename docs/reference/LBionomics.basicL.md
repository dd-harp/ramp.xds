# Mosquito bionomics for `basicL` (**L**)

Compute the baseline bionomic parameter values for the `basicL` module
and reset their effect sizes to 1.

## Usage

``` r
# S3 method for class 'basicL'
LBionomics(t, y, xds_obj, s)
```

## Arguments

- t:

  time

- y:

  dependent variables

- xds_obj:

  an **`xds`** model object

- s:

  the species index

## Value

an **`xds`** model object

## Note

Evaluate [F_psi](https://dd-harp.github.io/ramp.xds/reference/F_psi.md),
[F_phi](https://dd-harp.github.io/ramp.xds/reference/F_phi.md),
[F_xi](https://dd-harp.github.io/ramp.xds/reference/F_xi.md), and
[F_theta](https://dd-harp.github.io/ramp.xds/reference/F_theta.md) at
`t`. Store their values as `psi_t`, `phi_t`, `xi_t`, and `theta_t` at
`xds_obj$L_obj[[s]]`, and reset the corresponding `es_` fields to 1 per
habitat. Realized parameter values are set by
[LEffectSizes.basicL](https://dd-harp.github.io/ramp.xds/reference/LEffectSizes.basicL.md).

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

[basicL](https://dd-harp.github.io/ramp.xds/reference/basicL.md),
[LEffectSizes.basicL](https://dd-harp.github.io/ramp.xds/reference/LEffectSizes.basicL.md)
