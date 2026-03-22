# The `SI` module for the MY component

Implements the **MY** component using a simple SI (Susceptible-Infected)
model of adult mosquito infection dynamics.

## State Variables

- `M`:

  density of adult mosquitoes

- `Y`:

  density of infected adult mosquitoes

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

The demographic matrix, \\\Omega\\ is computed as \$\$\Omega =
\mbox{diag}\left(g + \sigma \mu \right) - K \cdot
\mbox{diag}\left(\sigma \left(1-\mu\right)\right)\$\$

## Inputs

**Emergence** – `Lambda` or \\\Lambda\\, is computed by the
**ML**-Interface using outputs of the **L** Component

**Net Infectiousness** – `kappa` or \\\kappa\\, is computed by the
**XY**-Interface using outputs of the **XH** Component

## Dynamics

\$\$ \begin{array}{rl} dM/dt &= \Lambda - \Omega \cdot M \\ dY/dt &=
fq\kappa(M-Y) - \Omega \cdot Y \\ \end{array}\$\$

## Net Infective Biting

This model incorporates mortality and dispersal through the EIP, but it
ignores the delay. The density of infectious mosquitoes is thus: \$\$Z =
e^{-\Omega \tau} \cdot Y\$\$ so net infective biting, the `fqZ` term, is
given by: \$\$fqZ = fq \left(e^{-\Omega \tau} \cdot Y\right)\$\$
