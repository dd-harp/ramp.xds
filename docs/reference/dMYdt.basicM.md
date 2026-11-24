# Compute the derivatives for `basicM` (**MY**)

Compute the derivatives for the `basicM` module: \$\$\begin{array}{rl}
dM/dt &= \Lambda - \Omega \cdot M \\\\ dP/dt &= f(M-P) - \Omega \cdot P
\end{array}\$\$

## Usage

``` r
# S3 method for class 'basicM'
dMYdt(t, y, xds_obj, s)
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

a [numeric](https://rdrr.io/r/base/numeric.html) vector of derivatives
in the module's state-variable order

## Note

Derivatives are returned in blocks for `M`, then `P`, each in patch
order.

## State Variables

- `M`:

  density of adult mosquitoes

- `P`:

  density of parous adult mosquitoes: those that have blood-fed at least
  once

## Dynamical Terms

- `Lambda` or \\(\Lambda\\):

  adult mosquito emergence rate, computed by
  [Emergence](https://dd-harp.github.io/ramp.xds/reference/Emergence.md)

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

- `eggsPerBatch` or \\(\Gamma\\):

  number of eggs per batch

After basic setup,
[change_MY_pars.basicM](https://dd-harp.github.io/ramp.xds/reference/change_MY_pars.basicM.md)
can be used to change static parameter values. The `basicM` module
evaluates bionomic parameter functions through
[MBionomics](https://dd-harp.github.io/ramp.xds/reference/MBionomics.md).

## ODE

Adult mosquitoes emerge from aquatic habitats at rate \\(\Lambda\\) and
disperse and die according to \\(\Omega\\). The ODE model is \$\$
\begin{array}{rl} dM/dt &= \Lambda-\Omega\cdot M \\\\\\\\ dP/dt &=
f(M-P)-\Omega\cdot P \end{array}\$\$ Here, \\(P\\) is the density of
mosquitoes that have blood-fed at least once.

## See also

[basicM](https://dd-harp.github.io/ramp.xds/reference/basicM.md),
[F_fqZ.basicM](https://dd-harp.github.io/ramp.xds/reference/F_fqZ.basicM.md),
[F_eggs.basicM](https://dd-harp.github.io/ramp.xds/reference/F_eggs.basicM.md)
