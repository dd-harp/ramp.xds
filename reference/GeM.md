# The `GeM` module for the **MY** component

`GeM` is a generalized, non-autonomous Macdonald model for mosquito
ecology and infection dynamics. It includes adult mosquito density and
emergence, spatial dynamics, and time-varying survival and dispersal
through a potentially time-varying extrinsic incubation period (EIP).

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

- `Lambda` or \\\Lambda\\:

  adult mosquito emergence rate, computed by
  [Emergence](https://dd-harp.github.io/ramp.xds/reference/Emergence.md)

- `kappa` or \\\kappa\\:

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

- `eip` or \\\tau\\:

  extrinsic incubation period — use
  [setup_eip](https://dd-harp.github.io/ramp.xds/reference/setup_eip.md)

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
\\\Omega(t)\\:
\$\$\Omega(t)=\mbox{diag}(g(t)+\sigma(t)\mu(t))-K\cdot\mbox{diag}(\sigma(t)(1-\mu(t))).\$\$
The parameter \\g\\ is the per-capita mortality rate while mosquitoes
are in a patch. Mosquitoes emigrate from patches at per-capita rates
\\\sigma\\, and a fraction \\\mu\\ of emigrating mosquitoes is lost. The
columns of the dispersal matrix \\K\\ sum to zero. For non-trivial
dispersal matrices, \\\mbox{diag}(K)=-1\\; the off-diagonal elements
describe the fractions of surviving emigrants that arrive in other
patches.

## EIP

Let \\\tau(t)\\ be the EIP for a mosquito infected at time \\t\\; it
becomes infectious at time \\t+\tau(t)\\. Let \\\tau'(t)\\ be the
elapsed time since infection for a mosquito that becomes infectious at
time \\t\\. These quantities satisfy \$\$\tau(t)=\tau'(t+\tau(t)) \qquad
\tau'(t)=\tau(t-\tau'(t)).\$\$ The `F_eip` function returns the lag
\\\tau'(t)\\ used by the delay equations. A time-varying EIP also
requires its derivative, computed by `dF_eip`.

## Accessory Variable

The matrix \\\Upsilon(t)\\ represents survival and dispersal through the
time-varying EIP, from \\t\_\tau=t-\tau'(t)\\ to \\t\\. In the
autonomous case it is \\e^{-\Omega\tau}\\. For time-varying demography
and EIP, it is computed as an accessory variable:
\$\$\frac{d\Upsilon}{dt}=\left(\Omega(t\_\tau)\left(1-\frac{d\tau'(t)}{dt}\right)-\Omega(t)\right)\cdot\Upsilon(t).\$\$
The initial matrix is set to \\\exp(-\Omega eip)\\. This variable is
internal to the model and is not an output orbit.

## Inputs

The **ML** interface supplies `Lambda` or \\\Lambda\\, the adult
mosquito emergence rate, using outputs of the **L** component. The
**XY** interface supplies `kappa` or \\\kappa\\, the net infectiousness,
using outputs of the **XH** component.

## Dynamics

A subscript \\\tau\\ denotes the value of a parameter, term, or variable
at time \\t-\tau'(t)\\: \$\$X\_\tau=X(t-\tau'(t)).\$\$ The derivatives
of the state variables are: \$\$ \begin{array}{rl} dM/dt &=
\Lambda-\Omega\cdot M \\\\ dP/dt &= f(M-P)-\Omega\cdot P \\\\ dY/dt &=
fq\kappa(M-Y)-\Omega\cdot Y \\\\ dZ/dt &=
\Upsilon\cdot(fq\kappa)\_\tau(M\_\tau-Y\_\tau)-\Omega\cdot Z
\end{array}\$\$ The implementation also evolves \\\Upsilon\\ and
accessory variables used to evaluate the delayed values of \\fq\kappa\\,
\\g\\, and \\\sigma\\.

## Egg Laying

The egg batch rate \\\nu\\ and the number of eggs per batch,
`eggs_per_batch`, determine the egg production rate per patch: \$\$G=\nu
M\cdot\texttt{eggs\\per\\batch}.\$\$
[F_eggs.GeM](https://dd-harp.github.io/ramp.xds/reference/F_eggs.GeM.md)
computes \\G\\, which is passed to
[EggLaying](https://dd-harp.github.io/ramp.xds/reference/EggLaying.md)
for distribution among aquatic habitats.

## Infective Biting

The human biting density, `fqM`, is the number of human bites per patch
per day; the infective biting density, `fqZ`, is the number of
infectious human bites per patch per day.
[F_fqM.GeM](https://dd-harp.github.io/ramp.xds/reference/F_fqM.GeM.md)
and
[F_fqZ.GeM](https://dd-harp.github.io/ramp.xds/reference/F_fqZ.GeM.md)
compute these quantities as \\f q M\\ and \\f q Z\\, respectively.
[compute_EIR](https://dd-harp.github.io/ramp.xds/reference/compute_EIR.md)
combines infective biting density with the biting distribution matrix
and resident-bite fraction to compute stratum-level `EIR`.
[F_hbr](https://dd-harp.github.io/ramp.xds/reference/F_hbr.md) uses
`fqM`, the same matrix, and resident-bite fraction to compute
stratum-level `HBR`.

## References

- Macdonald G (1952) The analysis of the sporozoite rate. Tropical
  Diseases Bulletin 49:569-586.

- Aron JL, May RM (1982) The population dynamics of malaria. Chapter 5
  in *The Population Dynamics of Infectious Diseases: Theory and
  Applications,* Springer, Boston, MA.

- Wu SL, *et al.* (2023) Spatial dynamics of malaria transmission. PLoS
  Computational Biology 19(6): e1010684.
  <https://doi.org/10.1371/journal.pcbi.1010684>

## See also

- Basic setup:
  [make_MY_obj_GeM](https://dd-harp.github.io/ramp.xds/reference/make_MY_obj_GeM.md),
  [setup_MY_obj.GeM](https://dd-harp.github.io/ramp.xds/reference/setup_MY_obj.GeM.md)

- Dynamics:
  [dMYdt.GeM](https://dd-harp.github.io/ramp.xds/reference/dMYdt.GeM.md)

- Biting and eggs:
  [F_fqZ.GeM](https://dd-harp.github.io/ramp.xds/reference/F_fqZ.GeM.md),
  [F_fqM.GeM](https://dd-harp.github.io/ramp.xds/reference/F_fqM.GeM.md),
  [F_eggs.GeM](https://dd-harp.github.io/ramp.xds/reference/F_eggs.GeM.md)

- Parameters:
  [get_MY_pars.GeM](https://dd-harp.github.io/ramp.xds/reference/get_MY_pars.GeM.md),
  [change_MY_pars.GeM](https://dd-harp.github.io/ramp.xds/reference/change_MY_pars.GeM.md)

- Variables:
  [make_MY_inits_GeM](https://dd-harp.github.io/ramp.xds/reference/make_MY_inits_GeM.md),
  [setup_MY_inits.GeM](https://dd-harp.github.io/ramp.xds/reference/setup_MY_inits.GeM.md),
  [parse_MY_orbits.GeM](https://dd-harp.github.io/ramp.xds/reference/parse_MY_orbits.GeM.md)
