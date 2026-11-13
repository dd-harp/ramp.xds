# The `SI` module for the **MY** component

This implements a model for basic mosquito ecology and an `SI`
compartmental model for mosquito infection dynamics

## State Variables

- `M`:

  density of adult mosquitoes

- `Y`:

  density of infected adult mosquitoes

The density of susceptible mosquitoes is \\M-Y\\

## Dynamics

**Dynamical Terms**

- Mosquitoes are born at the rate \\\Lambda,\\ which is a dynamical term
  computed by
  [Emergence](https://dd-harp.github.io/ramp.xds/reference/Emergence.md).

- The fraction of mosquitoes that get infected after blood feeding on a
  human, \\\kappa,\\ is a dynamical term computed by
  [Transmission](https://dd-harp.github.io/ramp.xds/reference/Transmission.md).

**Parameters**

- Mosquito survival and dispersal is given by the demographic matrix,
  \\\Omega\\

- Mosquitoes blood feed at the rate \\f.\\ The human fraction, \\q,\\ is
  the fraction of blood meals on human.

**Derivatives**

The model computes these derivatives:

\$\$ \begin{array}{rl} dM/dt &= \Lambda - \Omega \cdot M \\ dY/dt &=
fq\kappa(M-Y) - \Omega \cdot Y \\ \end{array}\$\$

## Infective Biting

To compute the infective biting density, this model configures a
parameter for the EIP, \\\tau.\\ Survival and dispersal through the EIP
is \$\$\Upsilon = e^{-\Omega \tau}.\$\$ Infective biting is given by
\$\$fqZ = \Upsilon \cdot \left( f q Y \right) \$\$

## Egg Laying

The egg laying rate.

## Parameters

- `f`:

  blood feeding rate

- `q`:

  human blood fraction

- `eip`:

  extrinsic incubation period (\\\tau\\)

- `g`:

  mosquito mortality rate

- `sigma`:

  emigration rate

- `mu`:

  emigration loss rate

- `K`:

  mosquito dispersal matrix

## Demography

- Mosquitoes emigrate from patches at the per-capita rates \\\sigma\\

- Some fraction of emigrating mosquitoes is lost (\\\mu\\), which could
  include so total population loss from a patch is \\g + \mu \sigma\\

The demographic matrix, \\\Omega\\ is computed as \$\$\Omega =
\mbox{diag}\left(g + \sigma \mu \right) - K \cdot
\mbox{diag}\left(\sigma \left(1-\mu\right)\right)\$\$

## Inputs

**Emergence** – `Lambda` or \\\Lambda\\, is computed by the
**ML**-Interface using outputs of the **L** Component

**Net Infectiousness** – `kappa` or \\\kappa\\, is computed by the
**XY**-Interface using outputs of the **XH** Component

## Net Infective Biting

This model incorporates mortality and dispersal through the EIP, but it
ignores the delay. The density of infectious mosquitoes is thus: \$\$Z =
e^{-\Omega \tau} \cdot Y\$\$ so net infective biting, the `fqZ` term, is
given by: \$\$fqZ = fq \left(e^{-\Omega \tau} \cdot Y\right)\$\$
