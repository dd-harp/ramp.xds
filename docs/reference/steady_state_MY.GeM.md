# Compute steady states for `GeM` (**MY**)

Compute steady-state mosquito densities for the `GeM` module.

## Usage

``` r
# S3 method for class 'GeM'
steady_state_MY(Lambda, kappa, xds_obj, s = 1)
```

## Arguments

- Lambda:

  the daily emergence rate of adult mosquitoes

- kappa:

  net infectiousness

- xds_obj:

  an **`xds`** model object

- s:

  the vector species index

## Value

a [list](https://rdrr.io/r/base/list.html)

## State Variables

- `M`:

  density of adult mosquitoes

- `P`:

  density of parous adult mosquitoes

- `Y`:

  density of infected adult mosquitoes

- `Z`:

  density of infectious adult mosquitoes

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

## Dynamics

A subscript \\(\tau\\) denotes the value of a parameter, term, or
variable at time \\(t-\tau'(t)\\): \$\$X\_\tau=X(t-\tau'(t)).\$\$ The
derivatives of the state variables are: \$\$ \begin{array}{rl} dM/dt &=
\Lambda-\Omega\cdot M \\\\\\\\ dP/dt &= f(M-P)-\Omega\cdot P \\\\\\\\
dY/dt &= fq\kappa(M-Y)-\Omega\cdot Y \\\\\\\\ dZ/dt &=
\Upsilon\cdot(fq\kappa)\_\tau(M\_\tau-Y\_\tau)-\Omega\cdot Z
\end{array}\$\$ The implementation also evolves \\(\Upsilon\\) and
accessory variables used to evaluate the delayed values of
\\(fq\kappa\\), \\(g\\), and \\(\sigma\\).

## See also

[GeM](https://dd-harp.github.io/ramp.xds/reference/GeM.md)
