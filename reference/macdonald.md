# The `macdonald` module for the **MY** component

This implements a Macdonald-style delay differential equation model for
adult mosquito ecology and infection dynamics, consistent with the model
published by George Macdonald in 1952. This formulation is closer to one
published by Aron and May (1982). Unlike the `SI` module, it includes
infectious mosquitoes as an explicit state variable and uses delayed
values to represent the extrinsic incubation period.

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

  egg batch rate — use
  [setup_nu](https://dd-harp.github.io/ramp.xds/reference/setup_nu.md)
  or
  [setup_egg_batch_rates](https://dd-harp.github.io/ramp.xds/reference/setup_nu.md)

- `eggsPerBatch` or \\\Gamma\\:

  number of eggs per batch

After basic setup,
[change_MY_pars.macdonald](https://dd-harp.github.io/ramp.xds/reference/change_MY_pars.macdonald.md)
can be used to change static parameter values. This module does not
support dynamic bionomic ports.

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
Survival and dispersal through the EIP are represented by
\$\$\Upsilon=e^{-\Omega\tau}.\$\$

## Blood Feeding

The blood feeding rate \\f\\ and human fraction \\q\\ are parameters of
the module. At baseline they are static. They should satisfy natural
constraints: \\f=0\\ if no blood hosts are available, \\q=0\\ if no
humans are available, and \\q=1\\ if humans are the only available
hosts. This module does not support the dynamic blood-feeding ports used
by `SI` and `GeM`.

## Dynamics

The system is described by coupled differential equations with a delay.
A subscript \\\tau\\ denotes a lagged value, for example
\\M\_\tau=M(t-\tau)\\.

\$\$ \begin{array}{rl} dM/dt &= \Lambda-\Omega\cdot M \\\\ dP/dt &=
f(M-P)-\Omega\cdot P \\\\ dY/dt &= fq\kappa(M-Y)-\Omega\cdot Y \\\\
dZ/dt &= \Upsilon\cdot(fq\kappa)\_\tau(M\_\tau-Y\_\tau)-\Omega\cdot Z
\end{array} \$\$

Here, \\P\\ is the density of mosquitoes that have blood-fed at least
once. The infected mosquito density \\Y\\ generates infectious
mosquitoes after the EIP, represented by the delayed term in the \\Z\\
equation.

## Infective Biting

Infective biting density is the number of infectious bites by mosquitoes
on humans in each patch per day. The density of infectious mosquitoes
\\Z\\ is computed from \\Y\\ after survival and dispersal through the
EIP.

- `fqZ`:

  infective biting density, the number of infectious bites per patch per
  day

- `fqM`:

  human biting density, the number of bites on humans per patch per day

[F_fqZ.macdonald](https://dd-harp.github.io/ramp.xds/reference/F_fqZ.macdonald.md)
and
[F_fqM.macdonald](https://dd-harp.github.io/ramp.xds/reference/F_fqM.macdonald.md)
compute these patch-level quantities.
[compute_EIR](https://dd-harp.github.io/ramp.xds/reference/compute_EIR.md)
combines `fqZ` with the biting distribution matrix and resident-bite
fraction to compute stratum-level EIR;
[F_hbr](https://dd-harp.github.io/ramp.xds/reference/F_hbr.md) computes
HBR from `fqM` in the same way. The resulting `EIR` and `HBR` orbits are
attached to the XH output during parsing.

## Egg Laying

The egg batch rate \\\nu\\ and the number of eggs per batch \\\Gamma\\
determine the egg production rate per patch: \$\$G=\nu\Gamma M.\$\$
[F_eggs.macdonald](https://dd-harp.github.io/ramp.xds/reference/F_eggs.macdonald.md)
computes \\G\\, which is passed to
[EggLaying](https://dd-harp.github.io/ramp.xds/reference/EggLaying.md)
for distribution among aquatic habitats.

## Note

This model is not capable of handling exogenous forcing by weather or
vector control. Use the `GeM` module instead.

## References

- Macdonald G (1952) The analysis of the sporozoite rate. Tropical
  Diseases Bulletin 49:569-586.

- Aron JL, May RM (1982) The population dynamics of malaria. Chapter 5
  in *The Population Dynamics of Infectious Diseases: Theory and
  Applications,* Springer, Boston, MA.

## See also

[make_MY_obj_macdonald](https://dd-harp.github.io/ramp.xds/reference/make_MY_obj_macdonald.md),
[setup_MY_obj.macdonald](https://dd-harp.github.io/ramp.xds/reference/setup_MY_obj.macdonald.md),
[dMYdt.macdonald](https://dd-harp.github.io/ramp.xds/reference/dMYdt.macdonald.md)
