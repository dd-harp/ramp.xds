# Mosquito bionomics for `basicM` (**MY**)

Implements
[MBionomics](https://dd-harp.github.io/ramp.xds/reference/MBionomics.md)
for models with no exogenous forcing

## Usage

``` r
# S3 method for class 'basicM'
MBionomics(t, y, xds_obj, s)
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

- `sigma` or \\\sigma\\:

  emigration rate — use
  [setup_sigma](https://dd-harp.github.io/ramp.xds/reference/setup_sigma.md)
  or
  [setup_emigration_rates](https://dd-harp.github.io/ramp.xds/reference/setup_sigma.md)

- `mu` or \\\mu\\:

  emigration loss fraction — use
  [setup_mu](https://dd-harp.github.io/ramp.xds/reference/setup_mu.md)
  or
  [setup_emigration_loss](https://dd-harp.github.io/ramp.xds/reference/setup_mu.md)

- `K` or `K_matrix`:

  mosquito dispersal matrix — use
  [setup_K_matrix](https://dd-harp.github.io/ramp.xds/reference/setup_K_matrix.md)

- `nu` or \\\nu\\:

  egg batch rate — use
  [setup_nu](https://dd-harp.github.io/ramp.xds/reference/setup_nu.md)
  or
  [setup_egg_batch_rates](https://dd-harp.github.io/ramp.xds/reference/setup_nu.md)

- `eggsPerBatch` or \\\Gamma\\:

  number of eggs per batch

After basic setup,
[change_MY_pars.basicM](https://dd-harp.github.io/ramp.xds/reference/change_MY_pars.basicM.md)
can be used to change static parameter values. The `basicM` module does
not implement dynamic bionomic ports.

## Survival and Dispersal

Mosquito survival and dispersal are described by the demographic matrix
\\\Omega\\. The parameter \\g\\ is the per-capita mortality rate while
mosquitoes are in a patch. Mosquitoes emigrate from patches at
per-capita rates \\\sigma\\, and a fraction \\\mu\\ of emigrating
mosquitoes is lost. The total rate of loss from a patch is
\$\$g+\mu\sigma.\$\$

The columns of the mosquito dispersal matrix \\K\\ sum to zero. For
non-trivial dispersal matrices, \\\mbox{diag}(K)=-1\\; the off-diagonal
elements describe the fractions of surviving emigrants that arrive in
other patches. The demographic matrix is
\$\$\Omega=\mbox{diag}(g+\sigma\mu)-K\cdot\mbox{diag}(\sigma(1-\mu)).\$\$

## See also

[basicM](https://dd-harp.github.io/ramp.xds/reference/basicM.md),
[MEffectSizes.basicM](https://dd-harp.github.io/ramp.xds/reference/MEffectSizes.basicM.md)
