# Update states for `SI` (**MY**)

Update the state variables for the `SI` module: \$\$\begin{array}{rl}
M_t &= \Lambda+e^{-\Omega}\cdot M\_{t-1} \\\\\\\\ P_t &=
e^{-\Omega}\cdot\left\[(1-e^{-fq})(M\_{t-1}-P\_{t-1})+P\_{t-1}\right\]
\\\\\\\\ Y_t &=
e^{-\Omega}\cdot\left\[(1-e^{-fq\kappa})(M\_{t-1}-Y\_{t-1})+Y\_{t-1}\right\]
\end{array}\$\$

## Usage

``` r
# S3 method for class 'SI'
Update_MYt(t, y, xds_obj, s)
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

## Dynamical Terms

- `Lambda` or \\(\Lambda\\):

  the number of adult mosquitoes emerging, per patch, per day, computed
  by
  [Emergence](https://dd-harp.github.io/ramp.xds/reference/Emergence.md)

- `kappa` or \\(\kappa\\):

  net infectiousness: the probability a mosquito becomes infected after
  blood feeding on a human, is computed by
  [Transmission](https://dd-harp.github.io/ramp.xds/reference/Transmission.md)

## Parameters

- `f`:

  blood feeding rates — use
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

- `K`:

  mosquito dispersal matrix — use
  [setup_K_matrix](https://dd-harp.github.io/ramp.xds/reference/setup_K_matrix.md)

- `nu` or \\(\nu\\):

  the oviposition rate: the number of egg batches laid in each patch,
  per mosquito, per day — use
  [setup_nu](https://dd-harp.github.io/ramp.xds/reference/setup_nu.md)
  or
  [setup_egg_batch_rates](https://dd-harp.github.io/ramp.xds/reference/setup_nu.md)

- `eggsPerBatch` or \\(\Gamma\\):

  the number of eggs per batch

After basic setup,
[change_MY_pars](https://dd-harp.github.io/ramp.xds/reference/change_MY_pars.md)
can be used for static parameters, or for parameters that are set up as
static ports (the default). Use advanced setup to modify dynamic ports.

## DTS

**Ecology**

Adult mosquitoes emerge from aquatic habitats at rate \\(\Lambda\\) and
disperse and die according to \\(\Omega\\). The one-day update in
Update_MYt.SI is: \$\$M_t = \Lambda + e^{-\Omega}\cdot M\_{t-1}\$\$ The
model also computes the density of parous adult mosquitoes: those that
have blood fed at least once: \$\$P_t =
e^{-\Omega}\cdot\left\[(1-e^{-fq})(M\_{t-1}-P\_{t-1}) +
P\_{t-1}\right\]\$\$

**Infection Dynamics**

Given the per-capita daily rate of human blood feeding \\((fq)\\) and
net infectiousness \\((\kappa)\\), the FoI for mosquitoes is
\\(fq\kappa\\). The density of infected mosquitoes is updated by:
\$\$Y_t = e^{-\Omega}\cdot\left\[(1-e^{-fq\kappa})(M\_{t-1}-Y\_{t-1}) +
Y\_{t-1}\right\]\$\$

## See also

[SI](https://dd-harp.github.io/ramp.xds/reference/SI.md),
[dMYdt.SI](https://dd-harp.github.io/ramp.xds/reference/dMYdt.SI.md)
