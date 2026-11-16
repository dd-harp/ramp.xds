# Mosquito bionomics for `GeM` (**MY**)

Implements
[MBionomics](https://dd-harp.github.io/ramp.xds/reference/MBionomics.md)
for models with no exogenous forcing

## Usage

``` r
# S3 method for class 'GeM'
MBionomics(t, y, xds_obj, s)
```

## Arguments

- t:

  current simulation time

- y:

  state vector

- xds_obj:

  an **`xds`** model object

- s:

  the vector species index

## Value

an **`xds`** object

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
  [setup_emigration_rates](https://dd-harp.github.io/ramp.xds/reference/setup_sigma.md)

- `mu` or \\(\mu\\):

  emigration loss fraction — use
  [setup_mu](https://dd-harp.github.io/ramp.xds/reference/setup_mu.md)
  or
  [setup_emigration_loss](https://dd-harp.github.io/ramp.xds/reference/setup_mu.md)

- `K` or `K_matrix`:

  mosquito dispersal matrix — use
  [setup_K_matrix](https://dd-harp.github.io/ramp.xds/reference/setup_K_matrix.md)

- `nu` or \\(\nu\\):

  egg batch rate in batches per mosquito per day — use
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

## EIP

Let \\(\tau(t)\\) be the EIP for a mosquito infected at time \\(t\\); it
becomes infectious at time \\(t+\tau(t)\\). Let \\(\tau'(t)\\) be the
elapsed time since infection for a mosquito that becomes infectious at
time \\(t\\). These quantities satisfy \$\$\tau(t)=\tau'(t+\tau(t))
\qquad \tau'(t)=\tau(t-\tau'(t)).\$\$ The `F_eip` function returns the
lag \\(\tau'(t)\\) used by the delay equations. A time-varying EIP also
requires its derivative, computed by `dF_eip`.

## See also

[GeM](https://dd-harp.github.io/ramp.xds/reference/GeM.md),
[MEffectSizes.GeM](https://dd-harp.github.io/ramp.xds/reference/MEffectSizes.GeM.md)
