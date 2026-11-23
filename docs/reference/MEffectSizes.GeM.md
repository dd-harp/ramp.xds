# Apply effect sizes for `GeM` (**MY**)

Implements
[MEffectSizes](https://dd-harp.github.io/ramp.xds/reference/MEffectSizes.md)
for the `GeM` module.

## Usage

``` r
# S3 method for class 'GeM'
MEffectSizes(t, y, xds_obj, s)
```

## Arguments

- t:

  time

- y:

  dependent variables

- xds_obj:

  an **`xds`** model object

- s:

  the vector species index

## Value

an **`xds`** model object

## Parameters

- `f`:

  blood feeding rate — use
  [setup_f](https://dd-harp.github.io/ramp.xds/reference/setup_f.md) or
  [setup_blood_feeding_rates](https://dd-harp.github.io/ramp.xds/reference/setup_f.md)

- `q`:

  human fraction — use
  [setup_q](https://dd-harp.github.io/ramp.xds/reference/setup_q.md) or
  [setup_human_fractions](https://dd-harp.github.io/ramp.xds/reference/setup_q.md)

- `g`:

  mosquito mortality rate — use
  [setup_g](https://dd-harp.github.io/ramp.xds/reference/setup_g.md) or
  [setup_mosquito_mortality](https://dd-harp.github.io/ramp.xds/reference/setup_g.md)

- `eip` or \\(\tau\\):

  extrinsic incubation period — use
  [setup_eip](https://dd-harp.github.io/ramp.xds/reference/setup_eip.md)

- `sigma` or \\(\sigma\\):

  emigration rate — use
  [setup_sigma](https://dd-harp.github.io/ramp.xds/reference/setup_sigma.md)
  or
  [setup_mosy_emigration](https://dd-harp.github.io/ramp.xds/reference/setup_sigma.md)

- `mu` or \\(\mu\\):

  emigration loss fraction — use
  [setup_mu](https://dd-harp.github.io/ramp.xds/reference/setup_mu.md)
  or
  [setup_emigration_loss](https://dd-harp.github.io/ramp.xds/reference/setup_mu.md)

- `K` or `K_matrix`:

  mosquito dispersal matrix — use
  [setup_K_matrix](https://dd-harp.github.io/ramp.xds/reference/setup_K_matrix.md)

- `nu` or \\(\nu\\):

  oviposition rate: the number of egg batches laid in each patch, per
  mosquito, per day — use
  [setup_nu](https://dd-harp.github.io/ramp.xds/reference/setup_nu.md)
  or
  [setup_egg_batch_rates](https://dd-harp.github.io/ramp.xds/reference/setup_nu.md)

- `eggs_per_batch`:

  number of eggs per batch

After basic setup,
[change_MY_pars.GeM](https://dd-harp.github.io/ramp.xds/reference/change_MY_pars.GeM.md)
can be used to change static parameter values. By default, bionomic
parameters are configured as static baselines; advanced setup can be
used to define dynamic ports.

## Survival and Dispersal

Mosquito survival and dispersal are described by the demographic matrix
\\(\Omega(t)\\):
\$\$\Omega(t)=\mbox{diag}(g(t)+\sigma(t)\mu(t))-K\cdot\mbox{diag}(\sigma(t)(1-\mu(t))).\$\$
The parameter \\(g\\) is the per-capita mortality rate while mosquitoes
are in a patch. Mosquitoes emigrate from patches at per-capita rates
\\(\sigma\\), and a fraction \\(\mu\\) of emigrating mosquitoes is lost.
The columns of the dispersal matrix \\(K\\) sum to zero. For non-trivial
dispersal matrices, \\(\mbox{diag}(K)=-1\\); the off-diagonal elements
describe the fractions of surviving emigrants that arrive in other
patches.

## See also

[GeM](https://dd-harp.github.io/ramp.xds/reference/GeM.md),
[MBionomics.GeM](https://dd-harp.github.io/ramp.xds/reference/MBionomics.GeM.md)
