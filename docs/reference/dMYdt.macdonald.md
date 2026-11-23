# Compute the derivatives for `macdonald` (**MY**)

Implements the delay differential equations for the `macdonald` module.

## Usage

``` r
# S3 method for class 'macdonald'
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

a [numeric](https://rdrr.io/r/base/numeric.html) vector

## State Variables

- `M`:

  density of adult mosquitoes

- `P`:

  density of parous adult mosquitoes

- `Y`:

  density of infected adult mosquitoes

- `Z`:

  density of infectious adult mosquitoes

## Dynamical Terms

- `Lambda` or \\(\Lambda\\):

  adult mosquito emergence rate, computed by
  [Emergence](https://dd-harp.github.io/ramp.xds/reference/Emergence.md)

- `kappa` or \\(\kappa\\):

  net infectiousness, computed by
  [Transmission](https://dd-harp.github.io/ramp.xds/reference/Transmission.md)

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

- `eggsPerBatch` or \\(\Gamma\\):

  number of eggs per batch

After basic setup,
[change_MY_pars.macdonald](https://dd-harp.github.io/ramp.xds/reference/change_MY_pars.macdonald.md)
can be used to change static parameter values. This module does not
support dynamic bionomic ports.

## Dynamics

The system is described by coupled differential equations with a delay.
A subscript \\(\tau\\) denotes a lagged value, for example
\\(M\_\tau=M(t-\tau)\\).

\$\$ \begin{array}{rl} dM/dt &= \Lambda-\Omega\cdot M \\\\\\\\ dP/dt &=
f(M-P)-\Omega\cdot P \\\\\\\\ dY/dt &= fq\kappa(M-Y)-\Omega\cdot Y
\\\\\\\\ dZ/dt &=
\Upsilon\cdot(fq\kappa)\_\tau(M\_\tau-Y\_\tau)-\Omega\cdot Z \end{array}
\$\$

Here, \\(P\\) is the density of mosquitoes that have blood-fed at least
once. The infected mosquito density \\(Y\\) generates infectious
mosquitoes after the EIP, represented by the delayed term in the \\(Z\\)
equation.

## See also

[macdonald](https://dd-harp.github.io/ramp.xds/reference/macdonald.md),
[F_fqZ.macdonald](https://dd-harp.github.io/ramp.xds/reference/F_fqZ.macdonald.md),
[F_eggs.macdonald](https://dd-harp.github.io/ramp.xds/reference/F_eggs.macdonald.md)
