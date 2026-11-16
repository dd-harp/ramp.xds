# Compute derivatives for `GeM` (**MY**)

Compute the derivatives for the generalized, non-autonomous Macdonald
model for mosquito ecology and infection dynamics. See
[GeM](https://dd-harp.github.io/ramp.xds/reference/GeM.md).

## Usage

``` r
# S3 method for class 'GeM'
dMYdt(t, y, xds_obj, s)
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

## EIP

Let \\(\tau(t)\\) be the EIP for a mosquito infected at time \\(t\\); it
becomes infectious at time \\(t+\tau(t)\\). Let \\(\tau'(t)\\) be the
elapsed time since infection for a mosquito that becomes infectious at
time \\(t\\). These quantities satisfy \$\$\tau(t)=\tau'(t+\tau(t))
\qquad \tau'(t)=\tau(t-\tau'(t)).\$\$ The `F_eip` function returns the
lag \\(\tau'(t)\\) used by the delay equations. A time-varying EIP also
requires its derivative, computed by `dF_eip`.

## Accessory Variable

The matrix \\(\Upsilon(t)\\) represents survival and dispersal through
the time-varying EIP, from \\(t\_\tau=t-\tau'(t)\\) to \\(t\\). In the
autonomous case it is \\(e^{-\Omega\tau}\\). For time-varying demography
and EIP, it is computed as an accessory variable:
\$\$\frac{d\Upsilon}{dt}=\left(\Omega(t\_\tau)\left(1-\frac{d\tau'(t)}{dt}\right)-\Omega(t)\right)\cdot\Upsilon(t).\$\$
The initial matrix is set to \\(\exp(-\Omega eip)\\). This variable is
internal to the model and is not an output orbit.

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

[GeM](https://dd-harp.github.io/ramp.xds/reference/GeM.md),
[F_fqZ.GeM](https://dd-harp.github.io/ramp.xds/reference/F_fqZ.GeM.md),
[F_eggs.GeM](https://dd-harp.github.io/ramp.xds/reference/F_eggs.GeM.md)
