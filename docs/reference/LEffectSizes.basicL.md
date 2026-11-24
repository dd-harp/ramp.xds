# Apply effect sizes for `basicL` (**L**)

Apply effect sizes to the baseline bionomic parameter values for the
`basicL` module.

## Usage

``` r
# S3 method for class 'basicL'
LEffectSizes(t, y, xds_obj, s)
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

At `xds_obj$L_obj[[s]]`, set `psi = psi_t * es_psi`,
`phi = phi_t * es_phi`, `xi = xi_t * es_xi`, and
`theta = theta_t * es_theta` per habitat. This function does not
evaluate parameter ports or reset effect sizes.

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
[LBionomics.basicL](https://dd-harp.github.io/ramp.xds/reference/LBionomics.basicL.md)
